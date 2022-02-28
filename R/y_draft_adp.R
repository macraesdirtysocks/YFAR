#' Get transaction draft ADP data from Yahoo! Fantasy API
#'
#' Yahoo drafts are accompanied by ADP's and this function returns them.
#' Values return include average_pick, average_round, average_cost and percent drafted.
#'
#' The return from this function is similar to `y_player_slate()` but returns the ADP's as additional columns.
#'
#' Function accepts a single game key, a single league key or a vector of player keys.
#'
#' Function is memoised.
#'
#' @param key A vector of game, league or player keys.
#' @param token_name Name used for assignment when creating token object with y_create_token().
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Suppress function print calls.
#'
#' @return A tibble
#' @examples
#' # Not Run
#' # y_draft_adp(key = "411", token_name = my_token, debug = FALSE)
#' # y_draft_adp(key = vector_of_player_keys, token_name = my_token, debug = FALSE)
#' @importFrom zeallot `%<-%`
#' @export
y_draft_adp <- memoise::memoise(function(key = NULL, token_name = NULL, debug = FALSE, quiet = TRUE) {

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    TOKEN                                 ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Standardize token name
  api_token <- token_name
  .token_check(token_name, api_token, name = .GlobalEnv)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    CHECKS                                ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  stopifnot(!is.null(key))
  stopifnot(!is.null(token_name))

  # Eligible key types
  e_key_types <- c("games", "leagues", "players")

  # Assign a resource to each key and count.
  # Function then selects most frequently occurring resource and assigns value to resource.
  # e_key_types = eligible key types.
  c(resource, key, .) %<-% .multiple_resource_key_check(key, e_key_types = e_key_types)

  # If resource is "games" or "leagues" select first element.
  # This function only accepts one league or game key.
  if (resource %in% c("games", "leagues") & vctrs::vec_size(key) > 1) {

    # Determine what game the key belongs to.
    key <- .game_key_assign_fn(key)

    cat(crayon::cyan("Mulitple game or leagues keys provided,", key, "selected.\n"), sep = " ")
  }

  if (!quiet) {
    cat(crayon::cyan("Resource is", resource, "\n"), sep = "")
  }
  if (!quiet) {
    cat(crayon::cyan("Keys are...\n", stringr::str_flatten(key, collapse = "\n")), sep = "\n")
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                  ARGUMENTS                               ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # resource assigned in if statement above.
  subresource <- switch(resource,
    "games" = "players",
    "leagues" = "players",
    "players" = "draft_analysis"
  )

  collection <- switch(resource,
    "games" = "draft_analysis",
    "leagues" = "draft_analysis"
  )

  uri_out <- switch(resource,
    "games" = "game_keys=",
    "leagues" = "league_keys=",
    "players" = "player_keys="
  )


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                FUNCTION DEFS                             ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Parse function dependant on resource.
  parse_fn <-
    switch(resource,
      "games" = {
        function(x) {
          .game_resource_parse_fn(x,
            pluck_args = list("game", 2, 1),
            fn = function(x) {
              purrr::map_df(x,
                .player_resource_parse_fn,
                pluck_args = list("player", 2),
                fn = function(x) .unlist_and_bind_fn(x)
              )
            }
          )
        }
      },
      "leagues" = {
        function(x) {
          .league_resource_parse_fn(x,
            pluck_args = list("league", 2, 1),
            fn = function(x) {
              purrr::map_df(x,
                .player_resource_parse_fn,
                pluck_args = list("player", 2),
                fn = function(x) .unlist_and_bind_fn(x)
              )
            }
          )
        }
      },
      "players" = {
        function(x) {
          .player_resource_parse_fn(x,
            pluck_args = list("player", 2),
            fn = function(x) .unlist_and_bind_fn(x)
          )
        }
      }
    )


  # Create a progress bar tick wrapper for parse function.
  parse_fn_tick <- function(x) {
    pb$tick()
    parse_fn(x)
  }



  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                      IF RESOURCE %IN% c(GAMES, LEAGUES                   ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (resource %in% c("games", "leagues")) {

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  WHILE LOOP                              ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # The Yahoo! api will return a max of 25 players per request and total number of
    # players in the full player slate is not known.  This loop will continue to get
    # the next 25 players as long as the the previous response contained 25 players.

    # Initialize an empty list to append responses.
    r_list <- list()

    # Initial while loop conditions.
    page_start <- 0
    count <- 25

    # While loop.  Continues until reponse contains less than 25 players which is assumed to be end player resource.
    while (count == 25) {

      message(crayon::cyan("Entering while loop..."))

      # Build uri - increment page_start by 25.
      uri <- stringr::str_glue(
        "https://fantasysports.yahooapis.com/fantasy/v2/{resource};{uri_out}{key}/{subresource};start={page_start}/{collection}?format=json"
      )

      if (!quiet) {
        message(crayon::cyan(uri))
        cat(crayon::cyan("Page start =", page_start, "\n", sep = " "))
      }

      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                                GET RESPONSE                              ----
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      r <-
        .y_get_response(uri, api_token)

      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                                   CONTENT                                ----
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Parse content.
      # unname list so appending to r_list won't overwrite itself.
      # The content will all be in a list with name "0" so each append duplicates itself and
      # without unnaming you end up with a list of the same 25 players.
      r_parsed <-
        .y_parse_response(r, "fantasy_content", resource) %>%
        unname()

      # Update while loop count.
      count <-
        purrr::pluck(r_parsed, 1, 1, 2, 1, "count")

      if (!quiet) {
        cat(crayon::cyan("Content length =", count, "\n", sep = " "))
      }

      # Update page start.
      page_start <- page_start + count

      # Append response to r_list for later parsing.
      r_list <- append(r_list, r_parsed, after = length(r_list))
    } # End while loop

    # General list pre-processing.
    # Preprocess aggined here which is outside the usual flow of the function to avoid ahving to do another switch
    # below before parsing.  IE. if resource == leagues r_parsed = r_list.

    preprocess <-
      r_list %>%
      list_pre_process_fn()

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                            IF RESOURCE == PLAYERS                        ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  } else if (resource == "players") {

    # Initial uri components
    uri_parsed <- structure(
      list(
        scheme = "https",
        hostname = "fantasysports.yahooapis.com/fantasy/v2",
        port = NULL,
        path = resource,
        query = list(format = "json"),
        params = NULL
      ),
      class = "url"
    )

    # Pack player keys into uri paths of length 25.
    key_path <-
      .uri_path_packer(key, 25)

    # Message
    if (!quiet) {
      cat(crayon::cyan("The keys provided were packed as following:\n"))
      message(crayon::cyan(key_path))
    }

    # Create paths using .uri_path_package(key) to pack player keys into groups of 25.
    uri_parsed$params <-
      stringr::str_c(uri_out, key_path, "/", subresource)

    # Build uris.
    uri <-
      httr::build_url(uri_parsed)

    # Map over uris with .y_get_response.
    r <-
      purrr::map(uri, .y_get_response, yahoo_token)

    # Check for bad responses.
    r_errors <-
      purrr::map_lgl(r, httr::http_error)

    # If there are errors in the responses then remove errors.
    if (sum(!purrr::map_lgl(r, httr::http_error)) <= 0) {
      stop(message(crayon::cyan("All requests returned errors. You may need a token refresh.")), call. = FALSE)
    } else {
      r <- r[!purrr::map_lgl(r, httr::http_error)]
    }

    # Map over uris with parse function.
    if (!vctrs::vec_is_empty(r)) {
      r_parsed <-
        purrr::map(r, .y_parse_response, "fantasy_content", resource)

      preprocess <-
        r_parsed %>%
        purrr::map(unname) %>%
        purrr::flatten() %>%
        list_pre_process_fn()
    } else {
      stop(message(crayon::cyan("No valid responses to parse.")), call. = FALSE)
    }
  } else {
    stop(message(crayon::cyan("Can't determine resource.")), call. = FALSE)
  }

  if (!debug) {
    cat(crayon::cyan("Parsing", length(preprocess), "responses...\n"))

    # Set progress bar length.
    pb <- progress::progress_bar$new(total = length(preprocess))

    # map parse function.
    df <-
      tryCatch(
        expr =
          preprocess %>%
            purrr::map_df(parse_fn_tick),
        error = function(e) {
          cat(
            crayon::cyan(
              "Function failed while parsing",
              resource,
              "resource.  Returning debug list.",
              sep = " "
            )
          )
        }
      )

    # Return df.
    if (tibble::is_tibble(df)) {
      return(df)
    }
  }

  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ##                                DEBUG RETURN                              ----
  # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  data_list <-
    structure(list(
      uri = uri,
      resource = resource,
      response = r,
      r_parsed = r_list
    ),
    class = "yahoo_fantasy_api"
    )

  return(data_list)
})

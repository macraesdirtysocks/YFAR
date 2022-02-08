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
  ##                            IF RESOURCE == GAMES                          ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (resource %in% c("games", "leagues")) {

    # Initialize empty list to store responses.
    r_list <- list()

    # while loop arguments
    page_start <- 0
    count <- 25


    ## ~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ WHILE LOOP OPEN  ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~

    cat(crayon::cyan(resource, "key provided, entering while loop...\n"), sep = " ")

    # while loop
    while (count == 25) {

      # Build uri - increment page_start by 25.
      uri <- stringr::str_glue(
        "https://fantasysports.yahooapis.com/fantasy/v2/{resource};{uri_out}{key}/{subresource};start={page_start}/{collection}?format=json"
      )

      if (!quiet) {
        print(uri)
      }

      r <- .y_get_response(uri, api_token)

      r_parsed <- .y_parse_response(r, "fantasy_content", resource, "0")

      # Append r_parsed to end of r_list
      r_list <- append(r_list, r_parsed, after = length(r_list))

      # Update count.  Loop continues until this value changes hopefully indicating the end of the players resource.
      count <- purrr::pluck(r_parsed, "game", 2, subresource, "count")

      # Update page start to get the next 25 players in the players resource.
      page_start <- page_start + count

      if (!quiet) {
        cat(crayon::cyan("player count =", page_start, "\n", sep = " "))
      }
    } # End while loop

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ WHILE LOOP CLOSE  ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ RESOURCE %in% c(GAMES, LEAGUES) RETURN  ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # if debug argument set to FALSE (default)
    if (!debug) {
      # Games meta.  This only operates on the first list because the game meta
      # should be the same for all list elements.
      game_meta <-
        r_list %>%
        purrr::pluck(1, 1) %>%
        dplyr::bind_cols() %>%
        dplyr::rename_with(~ paste(resource, .x, sep = "_"),
          .cols = !tidyselect::matches(glue::glue("^{resource}_"))
        )

      # Preprocess the list
      preprocess <-
        r_list %>%
        purrr::map(purrr::pluck, 2, subresource) %>%
        purrr::flatten() %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::set_names(nm = seq_along(.))

      # Print Pretty message about how many players we are getting.
      cat(crayon::cyan("Parsing", page_start, "player resources...\n", sep = " "))

      pb <- progress::progress_bar$new(total = length(preprocess))

      .draft_analysis_fn_tick <- function(x) {
        pb$tick()
        df <- .draft_analysis_fn(x)
        return(df)
      }

      # Create df by mapping over preprocess with function defined above.
      df <- tryCatch(
        expr =
          purrr::map_df(preprocess, .draft_analysis_fn_tick),
        error = function(e) {
          message(
            crayon::cyan(
              "Function failed while parsing games resource with .player_resource_parse_fn. Returning debug list."
            )
          )
                data_list <-
                        structure(list(
                                resource = resource,
                                content = r_list,
                                uri = uri,
                                total_players = page_start
                        ),
                        class = "yahoo_fantasy_api"
                        )
          return(data_list)
        }
      )

      # Return df
      if (tibble::is_tibble(df)) {
        return(df)
      }
    } else {
            data_list <-
                    structure(list(
                            resource = resource,
                            content = r_list,
                            uri = uri,
                            total_players = page_start
                    ),
                    class = "yahoo_fantasy_api"
                    )
      return(data_list)
    }

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ RESOURCE %in% c(GAMES, LEAGUES) RETURN  ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

    key_path <-
      .uri_path_packer(key, 25)

    if (!quiet) {
      cat("The keys provided were packed as following:\n")
      print(key_path)
    }

    # Create paths using .uri_path_package(key) to pack player keys into groups of 25.
    uri_parsed$params <-
      stringr::str_c(uri_out, key_path, "/", subresource)

    # Build uris.
    uri <- httr::build_url(uri_parsed)

    # Map over uris with GET function.
    r <- purrr::map(uri, .y_get_response, api_token)

    # Check for bad responses
    r_errors <- purrr::map_lgl(r, httr::http_error)

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
    } else {
      stop(message(crayon::cyan("No valid responses to parse.")), call. = FALSE)
    }

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ PLAYERS RESOURCE RETURN  ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!debug) {
      preprocess <-
        r_parsed %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::flatten()

      df <-
              tryCatch(
                      expr = purrr::map_df(preprocess, .draft_analysis_func),
                      error = function(e) {
                              message(
                                      crayon::cyan(
                                              "Function failed while parsing games resource with .player_resource_parse_fn. Returning debug list."
                                      )
                              )
                              data_list <-
                                      structure(list(
                                              resource = resource,
                                              content = r_list,
                                              uri = uri,
                                              total_players = page_start
                                      ),
                                      class = "yahoo_fantasy_api"
                                      )
                              return(data_list)
                      })

      if(tibble::is_tibble(df)){return(df)}

    } else {
      data_list <-
        structure(
          list(
            response = r,
            content = r_parsed,
            uri = uri
          ),
          class = "yahoo_fantasy_api"
        )

      return(data_list)
    }
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ END RESOURCE == PLAYERS  ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  } else {
    cat(crayon::cyan("Can't determine what type of key was provided.\n"))
  }
})

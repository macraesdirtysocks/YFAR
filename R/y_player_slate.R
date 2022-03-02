#' Get full slate of players from Yahoo! Fantasy API.
#'
#' Every game or league has a slate of players eligible to be on a team.  This function gets them for you.
#'
#' This function is not intended to get a subset of players, i.e. top 100 players.
#' Use `y_players()` for that case.
#'
#' Note: this function uses janitor::make_clean_names and as a result is a bit slow.
#'
#' @param key Game key or league key as a string in the form "000.l.0000".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param debug Print uri and page counts to console as functions runs.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble
#' @export
y_player_slate <- memoise::memoise(function(key = NULL, token_name = NULL, debug = FALSE, quiet = TRUE) {


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    TOKEN                                 ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  api_token <- token_name
  .token_check(token_name, api_token, name = .GlobalEnv)


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    CHECKS                                ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  stopifnot(!is.null(key))
  stopifnot(!is.null(api_token))


  # Eligible key types.
  e_key_types <- c("games", "leagues")

  # Assign a resource to each key and count.
  # Function then selects most frequently occurring resource and assigns value to resource.
  c(resource, key, .) %<-% .multiple_resource_key_check(key, e_key_types = e_key_types)

  # Determine what game the player_key belongs to.
  game_key <- .game_key_assign_fn(key)

  if (!quiet) {
    cat(crayon::cyan("game is", game_key, "\n"), sep = " ")
    cat(crayon::cyan("Resource is", resource, "\n"), sep = " ")
  }

  # Subset out player_keys belonging to game_key.
  # Function can't call multiple game resources.
  # vec_slice accounts for multiple leagues in the same game.
  key <-
    stringr::str_subset(string = key, pattern = game_key) %>%
    vctrs::vec_slice(1)


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                  ARGUMENTS                               ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  subresource <- "players"
  uri_out <-
      switch(
          resource,
          "games" = "game_keys",
          "leagues" = "league_keys"
  )

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                FUNCTION DEFS                             ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Vectorize seq.
  # This function will be used to sequence pages that return errors
  seq2 <- Vectorize(seq.default, vectorize.args = c("from"))


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

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                     URI                                  ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    uri <- glue::glue(
      "https://fantasysports.yahooapis.com/fantasy/v2/{resource};{uri_out}={key}/{subresource};start={page_start}?format=json"
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
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                      DF                                  ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!debug) {

    cat(crayon::cyan("Parsing", length(r_list), "responses...\n"))

    # General list pre-processing.
    preprocess <-
      r_list %>%
      list_pre_process_fn()

    # Set progress bar length.
    pb <- progress::progress_bar$new(total = length(preprocess))

    # Parse function dependant on resource.
    parse_fn <-
      switch(
          resource,
        "games" = {
          .game_resource_parse_fn
        },
        "leagues" = {
          .league_resource_parse_fn
        }
      )

    # Initial pluck location dependant on resource.
    initial_pluck <-
      switch(resource,
        "games" = list("game", 2, 1),
        "leagues" = list("league", 2, 1),
      )

    # Create a progress bar tick wrapper for parse function.
    parse_fn_tick <- function(x) {
      pb$tick()
      parse_fn(x, pluck_args = initial_pluck, fn = function(x) purrr::map_df(x, .player_resource_parse_fn))
    }

    # map parse function.
    df <-
      preprocess %>%
      purrr::map_df(parse_fn_tick)

    # Return df.
    return(df)
  }


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                DEBUG RETURN                              ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  data_list <-
    structure(
      list(
        count = count,
        page_start = page_start,
        r_parsed = r_list
      ),
      class = "yahoo_fantasy_api"
    )

  return(data_list)

})

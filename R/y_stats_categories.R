#' Get all existing stats categories for a game from the Yahoo! Fantasy API.
#'
#' Note not all stats that exist in a game will be used by your league.
#'
#' @param game_key 2 or 3 digit number of the game your league is playing.  Found with `y_games()`.
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param debug Returns a list of data uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A nested tibble with a game_key column and nested stats column.
#' @export
y_stats_categories <- function(game_key = NULL, token_name = NULL, debug = FALSE, quiet = TRUE) {


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                FUNCTION DEFS                             ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Parse the stats sub-resource of the game resource.
  subresource_parse_fn <- function(x) {

    # Remove top level stat list from in the list of stats
    x <- purrr::flatten(x)

    # Bind atomic elements.
    atomic <-
      x %>%
      purrr::keep(purrr::is_atomic) %>%
      dplyr::bind_cols()

    # Unlist and bind the list elements.
    the_lists <-
      x %>%
      purrr::keep(purrr::is_list) %>%
      purrr::map_dfc(.unlist_and_bind_fn)

    # Bind df.
    df <-
      dplyr::bind_cols(atomic, the_lists)

    # Return
    return(df)
  }


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    TOKEN                                 ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  api_token <- token_name
  .token_check(token_name, api_token, name = .GlobalEnv)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                  ARGUMENTS                               ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  resource <- "games"
  uri_out <- "game_keys="

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    CHECKS                                ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  stopifnot(!is.null(game_key))
  stopifnot(!is.null(api_token))

  # Check if keys are type league, remove FALSE and duplicates.
  key <- .single_resource_key_check(game_key, .game_key_check)

  if (!quiet) {
    cat(crayon::cyan("Resource is", resource, "\n"), sep = "")
  }
  if (!quiet) {
    cat(crayon::cyan("Keys are...\n", stringr::str_flatten(key, collapse = "\n")), sep = "\n")
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                     URI                                  ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  key_path <- .uri_path_packer(key, 25)

  uri <- stringr::str_c(
    "https://fantasysports.yahooapis.com/fantasy/v2/",
    resource,
    ";",
    uri_out,
    key_path,
    "/",
    c("stat_categories", "advanced_stat_categories"),
    "?format=json",
    sep = ""
  )

  if (!quiet) {
    cat(crayon::cyan("uri is...\n", uri), sep = "\n")
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                GET RESPONSE                              ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  r <- purrr::map(uri, .y_get_response, api_token)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                          CHECK RESPONSE FOR ERRORS                       ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (sum(!purrr::map_lgl(r, httr::http_error)) <= 0) {
    stop(message(crayon::cyan("All requests returned errors. You may need a token refresh.")), call. = FALSE)
  }

  r <- r[!purrr::map_lgl(r, httr::http_error)]

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                PARSE CONTENT                             ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  r_parsed <-
    purrr::map(r, .y_parse_response, "fantasy_content", resource)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                      DF                                  ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  if (!debug) {

    # Content is a list of 2 lists.
    # Element 1 contains the standard stats and element 2 contains the advanced stats.

    # General pre-processing.
    preprocess <-
      r_parsed %>%
      purrr::map(list_pre_process_fn)

    # Parse the standard stats.
    standard_stats <-
      preprocess %>%
      purrr::pluck(1) %>%
      purrr::map_df(.game_resource_parse_fn, list("game", 2, 1, 1), function(x) purrr::map_df(x, subresource_parse_fn))

    # Parse the advanced stats.
    advanced_stats <-
      preprocess %>%
      purrr::pluck(2) %>%
      purrr::map_df(.game_resource_parse_fn, list("game", 2, 1, 2), function(x) purrr::map_df(x, subresource_parse_fn))

    # Bind df.
    df <-
      dplyr::bind_rows(standard_stats, advanced_stats)

    return(df)
  }


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                DEBUG RETURN                              ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  data_list <-
    structure(
      list(
        resource = resource,
        response = r,
        content = r_parsed,
        uri = uri
      ),
      class = "yahoo_fantasy_api"
    )

  return(data_list)
}

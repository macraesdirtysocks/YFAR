#' Get standings data from Yahoo! Fantasy API
#'
#' Get standings data, divison data playoff seed and team matchup results.
#' This function does not return individual category win data form H2H leagues.
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return A tibble with nested columns
#' @export
y_standings <- function(league_id = NULL, token_name = NULL) {

    api_token <- token_name

    uri <- stringr::str_c(
        "https://fantasysports.yahooapis.com/fantasy/v2",
        "league",
        league_id,
        "standings?format=json",
        sep = "/"
    )

    league_id_check(league_id)
    stopifnot(token_check() == 1)

    r <- y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <- y_parse_response(r) %>%
        purrr::pluck("league", 2, "standings", 1, "teams") %>%
        purrr::map(purrr::pluck, "team") %>%
        purrr::compact()

    team_meta <- purrr::map_df(r_parsed, team_meta_func)

    stats <- purrr::map_df(r_parsed, stats_data_func)

    df <- dplyr::bind_cols(team_meta, stats)

    return(df)

}

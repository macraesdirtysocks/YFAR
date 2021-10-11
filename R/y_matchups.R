#' Get matchup stats and results data from Yahoo! Fantasy API
#'
#' y_matchups() gets matchup stats and results for a given league.
#' It returns a tibble of nested columns with a length equal to the number of teams
#' in the league.
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a [tibble][tibble::tibble-package] with nested columns
#' @export
y_matchups <- function(league_id = NULL, token_name = NULL) {

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    uri <-  stringr::str_c(
        "https://fantasysports.yahooapis.com/fantasy/v2/league",
        league_id,
        "teams",
        "matchups?format=json",
        sep = "/"
    )

    r <- .y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "teams")

    team_info <-
        .team_info_func(r_parsed)

    df <-
        dplyr::full_join(team_info, .week_info_func(r_parsed), by = "team") %>%
        dplyr::full_join(.stat_winner_func(r_parsed), by = "team") %>%
        dplyr::full_join(.week_stats_func(r_parsed), by = "team")

    return(df)

}

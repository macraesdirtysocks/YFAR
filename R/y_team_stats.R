#' Get team level stats for scoring categories
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a list
#' @export
y_team_stats <- function(league_id = NULL, token_name = NULL){

    resource <- "league"
    subresource1 <- "teams"
    subresource2 <- "stats"

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, league_id, subresource1, subresource2, sep = "/"),
            query = "format=json"
        )

    r <-
        .y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "teams")

    df <-
        dplyr::bind_cols(
            purrr::map_df(r_parsed, .team_meta_func, "team", 1),
            purrr::map_df(r_parsed, .team_stats_func)
        )

    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                matchup_data = df
            ),
            class = "yahoo_fantasy_api")

    return(data_list)
}

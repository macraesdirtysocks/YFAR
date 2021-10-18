#' Get standings data from Yahoo! Fantasy API
#'
#' Get standings data, divison data playoff seed and team matchup results.
#' This function does not return individual category win data form H2H leagues.
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a list
#' @export
y_standings <- function(league_id = NULL, token_name = NULL) {

    resource <- "league"
    subresource <- "standings"

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, league_id, subresource, sep = "/"),
            query = "format=json"
        )

    r <-
        .y_get_response(uri, api_token)

    # httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "standings", 1, "teams") %>%
        purrr::map(purrr::pluck, "team") %>%
        purrr::compact()

    team_meta <-
        purrr::map_df(r_parsed, .team_meta_func, 1)

    stats <-
        purrr::map_df(r_parsed, .stats_data_func)

    df <-
        dplyr::bind_cols(team_meta, stats)

    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                data = df
            ),
            class = "yahoo_fantasy_api")

    return(data_list)

}

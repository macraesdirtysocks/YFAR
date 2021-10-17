#' Get draft results
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a list
#' @export
y_draft_results <- function(league_id = NULL, token_name = NULL) {

    resource <- "league"
    subresource <- "draftresults"

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    uri <-
        httr::modify_url(
        url = "https://fantasysports.yahooapis.com",
        path = paste("fantasy/v2",resource, league_id, subresource, sep = "/"),
        query = "format=json"
    )

    r <-
        .y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "draft_results")

    df <-
        r_parsed %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map(1, purrr::flatten) %>%
        purrr::map_df(dplyr::bind_rows)

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

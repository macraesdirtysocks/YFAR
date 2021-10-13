#' Get team rosters
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
y_rosters <- function(league_id = NULL, token_name = NULL) {

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    uri <-
        stringr::str_c(
            "https://fantasysports.yahooapis.com/fantasy/v2/league",
            league_id,
            "teams",
            "roster?format=json",
            sep = "/"
        )

    r <-
        .y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "teams")

    df <- r_parsed %>%
        purrr::map(purrr::pluck, "team") %>%
        purrr::map_depth(2, purrr::compact) %>%
        # element 1 extract team_id and name
        purrr::map(purrr::map_at, 1, rlang::squash) %>%
        purrr::map(purrr::map_at, 1, `[`, c("team_id", "name")) %>%
        # rename team_id and name to "organization_id", "team_name"
        purrr::map(purrr::map_at, 1, purrr::set_names, c("organization_id", "team_name")) %>%
        purrr::map(purrr::map_at, 1, dplyr::bind_rows) %>%
        purrr::map(purrr::map_at, 2, purrr::pluck, "roster", "0", "players") %>%
        purrr::map(purrr::map_at, 2, purrr::keep, purrr::is_list) %>%
        purrr::map(purrr::map_at, 2, purrr::map, purrr::pluck, "player") %>%
        purrr::map(purrr::map_at, 2, purrr::compact) %>%
        # formatting for player meta at element 2, 0, 2, 1
        purrr::map(purrr::map_at, 2, purrr::map_depth, 2, purrr::compact) %>%
        purrr::map(purrr::map_at, 2, purrr::map_depth, 2, purrr::flatten) %>%
        purrr::map(purrr::map_at, 2, purrr::map, purrr::map_at, 1, purrr::map_at, "name", purrr::pluck, "full") %>%
        purrr::map(purrr::map_at, 2, purrr::map, purrr::map_at, 1, purrr::map_at, "eligible_positions", purrr::flatten) %>%
        purrr::map(purrr::map_at, 2, purrr::map, purrr::map_at, 1, purrr::map_at, "eligible_positions", ~purrr::set_names(., paste("position_", seq_along(.)))) %>%
        purrr::map(purrr::map_at, 2, purrr::map, purrr::map_at, 1, purrr::flatten) %>%
        purrr::map(purrr::map_at, 2, purrr::map, purrr::map_at, 2, `[`, -1) %>%
        # formatting for player meta at element 2, 0, 2, 2
        purrr::map(purrr::map_at, 2, purrr::map, purrr::map_at, 2, purrr::flatten) %>%
        purrr::map(purrr::map_at, 2, purrr::map, purrr::map_at, 2, purrr::set_names, c("lineup_position", "lineup_position_is_flex")) %>%
        # formatting for player meta at element 2, 0, 2, 2
        # create df
        purrr::map(purrr::map_at, 2, purrr::map_depth, 2, purrr::flatten_df) %>%
        purrr::map(purrr::map_at, 2, purrr::map_df, dplyr::bind_cols) %>%
        purrr::map_df(dplyr::bind_cols) %>%
        dplyr::group_by(organization_id) %>%
        dplyr::mutate(player_num = seq(1:n())) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(organization_id = stringr::str_pad(organization_id, width = 2, side = "left", pad = "0") %>%
                          paste("o", ., sep = "."))

    return(df)

}

#' Get team data of a given Yahoo! Fantasy league.
#'
#' Teams in a league make moves, have co-managers, have a waiver prioity etc.  This function
#' returns all team level meta data from the Yahoo! Fantasy API.
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a [tibble][tibble::tibble-package] with both nested and un-nested columns.
#' @export
y_teams <- function(league_id = NULL, token_name = NULL){

    api_token <- token_name

    league_id_check(league_id)
    stopifnot(token_check() == 1)

    uri <- stringr::str_c(
        "https://fantasysports.yahooapis.com/fantasy/v2",
        "league",
        league_id,
        "teams?format=json",
        sep = "/")

    r <- y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <- y_parse_response(r, "fantasy_content", "league", 2, "teams") %>%
        purrr::map(purrr::pluck, "team", 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map(purrr::flatten)

    team_meta <-
        r_parsed %>%
        purrr::map(purrr::keep, purrr::negate(purrr::is_list)) %>%
        purrr::map(purrr::modify_if, is.numeric, as.character) %>%
        dplyr::bind_rows()

    team_logos <-
        r_parsed %>%
        purrr::map(purrr::pluck, "team_logos", 1, "team_logo") %>%
        purrr::map_df(dplyr::bind_rows) %>%
        purrr::set_names(~stringr::str_c("team", ., sep = "_"))

    managers <-
        r_parsed %>%
        purrr::map(purrr::pluck, "managers") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map(purrr::set_names, ~stringr::str_c(., seq_along(.), sep = "_")) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), tidyr::replace_na, "0")) %>%
        dplyr::group_nest(team, .key = "manager_info", keep = FALSE) %>%
        dplyr::select(-c(team))

    df <- dplyr::bind_cols(team_meta, team_logos, managers)

    return(df)
}

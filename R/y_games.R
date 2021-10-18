#' Get all Yahoo! Fantasy hockey meta data.
#'
#' Returns a tibble containing all the meta data for all fantasy hockey
#' games and leagues participated in by the logged in user including those which
#' are currently active.
#'
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a list
#' @export
y_games <- function(token_name = NULL) {

    resource <- "users"
    subresource1 <- "games"
    subresource2 <- "leagues"

    api_token <- token_name

    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, sep = "/"),
            params = paste("use_login=1", subresource1, subresource2, sep = "/"),
            query = "format=json"
        )

    .token_check(token_name, api_token, name = .GlobalEnv)

    r <-
        .y_get_response(uri, api_token)

    # httr::stop_for_status(r, task = "Authroize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "users", "0", "user", 2, "games")

    df <-
        r_parsed %>%
        purrr::map(purrr::pluck, "game") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::keep(purrr::every, purrr::is_list) %>%
        ### game meta
        purrr::map_depth(1, purrr::map_at, 1, dplyr::bind_rows) %>%
        ### specific league data
        purrr::map_depth(1, purrr::map_at, 2, purrr::pluck, "leagues") %>%
        purrr::map_depth(1, purrr::map_at, 2, purrr::keep, purrr::is_list) %>%
        purrr::map_depth(1, purrr::map_at, 2, purrr::map, purrr::pluck, "league", 1) %>%
        purrr::map_depth(
            1,
            purrr::map_at,
            2,
            purrr::map,
            ~ dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), as.character))
        ) %>%
        purrr::map_depth(1, purrr::map_at, 2, dplyr::bind_rows) %>%
        purrr::map_depth(1,
                         purrr::map_at,
                         1,
                         purrr::set_names,
                         nm = ~paste("meta", ., sep = "_")) %>%
        purrr::map_df(dplyr::bind_cols) %>%
        dplyr::relocate(c(league_key, league_id), .after = "meta_type") %>%
        dplyr::filter(meta_name == "Hockey")

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

y_games <- function() {

    games_link <- "https://fantasysports.yahooapis.com/fantasy/v2/users;use_login=1/games/leagues?format=json"

    token_check <-
        purrr::map(.x = ls(name = .GlobalEnv), .f = get) %>%
        purrr::map_chr(.f = janitor::describe_class) %>%
        stringr::str_detect(pattern = "Token") %>%
        sum()

    stopifnot(token_check == 1)

    r <-
        httr::GET(
            url = games_link,
            httr::add_headers(
                Authorization = stringr::str_c(
                    "Bearer", yahoo_token$credentials$access_token, sep = " ")
                ))

    httr::stop_for_status(r)

    r_parsed <-
        jsonlite::fromJSON(
            httr::content(r, as = "text", encoding = "utf-8"),
            simplifyVector = FALSE) %>%
        purrr::pluck("fantasy_content", "users", "0", "user", 2, "games")

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
        purrr::map_depth(1, purrr::map_at, 2, purrr::map, ~ dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), as.character))) %>%
        purrr::map_depth(1, purrr::map_at, 2, dplyr::bind_rows) %>%
        purrr::map_depth(1, purrr::map_at, 1, purrr::set_names, nm = ~ stringr::str_c("meta", ., sep = "_")) %>%
        purrr::map_df(dplyr::bind_cols) %>%
        dplyr::relocate(c(league_key, league_id), .after = "meta_type") %>%
        dplyr::filter(meta_name == "Hockey")

    return(df)

}

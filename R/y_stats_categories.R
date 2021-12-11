#' Get all exisiting stats categories for a game.
#'
# Note: Not all stats that exist in a game will be used by your league.
#'
#' @param game_key 3 digit number of the game your league is playing.  Found with `y_games()`.
#' @param token_name Assigned object name used when creating token with `y_create_token()`.
#'
#' @return a tibble
#' @export
y_stats_categories <- function(game_key = 411, token_name = NULL){

    basic_stats <-
        .y_get_response(
            paste(
                "https://fantasysports.yahooapis.com/fantasy/v2/game",
                game_key,
                "stat_categories?format=json",
                sep = "/"),
            token_name
        ) %>%
        .y_parse_response("fantasy_content") %>%
        purrr::pluck("game", 2, "stat_categories", "stats") %>%
        purrr::map(purrr::pluck, "stat") %>%
        purrr::map(purrr::keep, purrr::negate(purrr::is_list)) %>%
        purrr::map_df(dplyr::bind_cols) %>%
        dplyr::mutate(stat_id = as.character(stat_id))

    advanced_stats <-
        .y_get_response(
            paste("https://fantasysports.yahooapis.com/fantasy/v2/game",
                  game_key,
                  "advanced_stat_categories?format=json",
                  sep = "/"),
            token_name) %>%
        .y_parse_response("fantasy_content") %>%
        purrr::pluck("game", 2, "advanced_stat_categories", "stats") %>%
        purrr::map(purrr::pluck, "stat") %>%
        purrr::map(purrr::keep, purrr::negate(purrr::is_list)) %>%
        purrr::map_df(dplyr::bind_cols)

    stats <- dplyr::bind_rows(basic_stats, advanced_stats) %>%
        dplyr::mutate(display_name = janitor::make_clean_names(display_name))

    return(stats)
}


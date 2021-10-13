#' Get player data from Yahoo! Fantasy API
#'
#' Function returns a tibble containing info on all players in your leagues universe.
#' By default the players are sorts by "Actual Rank"
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @export
y_scoreboard <- function(league_id, token_name) {

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    uri <- stringr::str_c(
        "https://fantasysports.yahooapis.com/fantasy/v2/league",
        league_id,
        "teams/matchups?format=json",
        sep = "/"
    )

    r <- .y_get_response(uri, api_token)



    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <- .y_parse_response(r, "fantasy_content")

    return(r_parsed)
    #
    # season_length <- leagues[leagues$league_key == league_id, "end_week"] %>% as.integer()
    #
    # all <- seq(from = 1, to = season_length, by = 1)
    #
    # number_of_weeks <- if(is.numeric(week)){
    #     all[week]
    # } else {
    #
    #     all
    # }
    #
    # cat("getting results for weeks", number_of_weeks)
    #
    # r <-
    #     httr::GET(
    #         url = str_c("https://fantasysports.yahooapis.com/fantasy/v2/league",
    #                     league_id,
    #                     glue("scoreboard","{str_c('week=', number_of_weeks, collapse = ';')}?format=json", .sep = ";"),
    #                     sep = "/"),
    #         add_headers(
    #             Authorization = str_c("Bearer",
    #                                   yahoo_token$credentials$access_token, sep = " ")
    #         )
    #     )
    #
    #
    # httr::stop_for_status(r, task = "Authroize, refresh token with yahoo_token$refresh() and try again")
    #
    # r_parsed <-
    #     jsonlite::fromJSON(content(r, as = "text", encoding = "utf-8"),
    #                        simplifyVector = FALSE) %>%
    #     pluck("fantasy_content",
    #           "league",
    #           2,
    #           "scoreboard",
    #           "0",
    #           "matchups") %>%
    #     flatten() %>%
    #     keep(is_list)
    #
    #
    # weekly_results <- list(
    #     team_1 = list(
    #         week_info = r_parsed %>%
    #             map_df(keep, is_atomic),
    #
    #         stat_winners = r_parsed %>%
    #             map(pluck, "stat_winners") %>%
    #             map_depth(2, bind_rows) %>%
    #             map(bind_rows) %>%
    #             map_df(nest, stat_winners = everything()),
    #
    #         team_info = r_parsed %>%
    #             map(pluck, "0", "teams", "0", "team", 1) %>%
    #             map(compact) %>%
    #             map(flatten) %>%
    #             map_if(is_list, unlist) %>%
    #             set_names(
    #                 nm = str_replace_all,
    #                 pattern = "\\.",
    #                 replacement = "_"
    #             ) %>%
    #             map_df(bind_rows),
    #
    #         team_stats = r_parsed %>%
    #             map(pluck, "0", "teams", "0", "team", 2, "team_stats", "stats") %>%
    #             map_depth(2, bind_rows) %>%
    #             map(bind_rows) %>%
    #             map_df(nest, team_stats = everything()),
    #
    #         week_points = r_parsed %>%
    #             map(pluck, "0", "teams", "0", "team", 2, "team_points") %>%
    #             map_df(~ keep(., names(.) == "total")),
    #
    #         games_played = r_parsed %>%
    #             map(
    #                 pluck,
    #                 "0",
    #                 "teams",
    #                 "0",
    #                 "team",
    #                 2,
    #                 "team_remaining_games",
    #                 "total"
    #             ) %>% map_df(~ keep(., names(.) == "completed_games"))
    #     ),
    #
    #     team_2 = list(
    #         week_info = r_parsed %>%
    #             map_df(keep, is_atomic),
    #
    #         stat_winners = r_parsed %>%
    #             map(pluck, "stat_winners") %>%
    #             map_depth(2, bind_rows) %>%
    #             map(bind_rows) %>%
    #             map_df(nest, stat_winners = everything()),
    #
    #         team_info = r_parsed %>%
    #             map(pluck, "0", "teams", "1", "team", 1) %>%
    #             map(compact) %>%
    #             map(flatten) %>%
    #             map_if(is_list, unlist) %>%
    #             set_names(
    #                 nm = str_replace_all,
    #                 pattern = "\\.",
    #                 replacement = "_"
    #             ) %>%
    #             map_df(bind_rows),
    #
    #         team_stats = r_parsed %>%
    #             map(pluck, "0", "teams", "1", "team", 2, "team_stats", "stats") %>%
    #             map_depth(2, bind_rows) %>%
    #             map(bind_rows) %>%
    #             map_df(nest, team_stats = everything()),
    #
    #         week_points = r_parsed %>%
    #             map(pluck, "0", "teams", "1", "team", 2, "team_points") %>%
    #             map_df( ~ keep(., names(.) == "total")),
    #
    #         games_played = r_parsed %>%
    #             map(
    #                 pluck,
    #                 "0",
    #                 "teams",
    #                 "1",
    #                 "team",
    #                 2,
    #                 "team_remaining_games",
    #                 "total"
    #             ) %>% map_df(~ keep(., names(.) == "completed_games"))
    #     )
    # )
    #
    # df <-  map_df(weekly_results, bind_cols)
    #
    # return(df)

}

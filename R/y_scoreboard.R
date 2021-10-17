#' Get scoreboard stats and results data from Yahoo! Fantasy API
#'
#' `y_scoreboard()` gets scoreboard stats and results for a given league and week.
#' It returns a list with the meta data as well as tibble containing the scoreboard data
#' in the element `scoreboard_data`.
#'
#' `y_scoreboard()` takes a league id and returns matchup data for all teams in requested week.
#' `y_matchups()` takes a team id and returns matchup data for requested weeks and team.
#'
#' @param league_id team id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name assigned object name used when creating token with y_create_token().
#' @param week the week of fantasy season to return. Default NULL will return current week of season.
#'
#' @return a list
#' @export
y_scoreboard <- function(league_id = NULL, token_name = NULL, week = NULL) {

    resource <- "league"
    subresource <- "scoreboard"

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    stopifnot(is.null(week) | is.numeric(week))

    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, league_id, subresource, sep = "/"),
            param = glue::glue("weeks={week}"),
            query = "format=json"
        )

    r <-
        .y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "scoreboard")



    #......................INITIALIZE EMPTY LIST.....................


    scoreboard_list <- list(
        intitial_data = NULL,
        matchup_meta = NULL,
        stat_winners = NULL,
        team_data =
            list(
                meta = NULL,
                stats = NULL,
                week_points = NULL,
                game_counts = NULL
            )
    )


    #.......................LIST PREPROCESSING.......................

    # Remove top levels of the list to simplify parsing.
    # This is the base origin for the parsing code below.

    scoreboard_list$inital_data <-
        r_parsed %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map(purrr::pluck, "matchups") %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        # re number weeks
        purrr::set_names(nm = seq_along(.)) %>%
        #re number matchups
        purrr::map(~purrr::set_names(., nm = seq_along(.))) %>%
        # purrr::pluck matchup element from each week
        purrr::map_depth(2, purrr::pluck, "matchup")


    #..........................MATCHUP META..........................


    scoreboard_list$matchup_meta <-
        scoreboard_list$inital_data %>%
        purrr::map_depth(2, purrr::keep, purrr::is_bare_atomic) %>%
        purrr::map(dplyr::bind_rows, .id = "matchup_id") %>%
        dplyr::bind_rows()


    #..........................STAT WINNERS..........................


    scoreboard_list$stat_winners <-
        scoreboard_list$inital_data %>%
        purrr::map_depth(2, purrr::pluck, "stat_winners") %>%
        # compact() %>%
        purrr::map_depth(3, purrr::pluck, "stat_winner") %>%
        purrr::map_depth(3, purrr::map_if, purrr::negate(is.character), as.character) %>%
        purrr::map_depth(3, dplyr::bind_rows) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::set_names(nm = seq_along(.)) %>%
        purrr::map(purrr::map_if, purrr::negate(purrr::is_empty), tidyr::pivot_wider,
                      id_cols = stat_id,
                      names_from = stat_id,
                      values_from = "winner_team_key",
                      names_glue = "stat_id_{ stat_id }_winner"
        ) %>%
        purrr::map(dplyr::bind_rows, .id = "matchup_id") %>%
        dplyr::bind_rows(.id = "week")


    #.........................TEAM META DATA.........................


    scoreboard_list$team_data$meta <-
        scoreboard_list$inital_data %>%
        purrr::map_depth(2, purrr::pluck, "0", "teams") %>%
        purrr::map_depth(2, purrr::keep, purrr::is_list) %>%
        purrr::map_depth(3, purrr::pluck, "team") %>%
        purrr::map_depth(3, purrr::pluck, 1) %>%
        purrr::map_depth(3, magrittr::extract, 1:3) %>%
        purrr::map_depth(3, purrr::map_at, 1, purrr::map_if, purrr::negate(is.character), as.character) %>%
        purrr::map_depth(2, ~purrr::set_names(., nm = seq_along(.))) %>%
        purrr::map_depth(3, dplyr::bind_cols) %>%
        purrr::map_depth(2, dplyr::bind_rows, .id = "matchup_team") %>%
        purrr::map(dplyr::bind_rows, .id = "matchup_id") %>%
        dplyr::bind_rows(.id = "week")


    #........................TEAM STATS DATA.........................


    scoreboard_list$team_data$stats <-
        scoreboard_list$inital_data %>%
        purrr::map_depth(2, purrr::pluck, "0", "teams") %>%
        purrr::map_depth(2, purrr::keep, purrr::is_list) %>%
        purrr::map_depth(3, purrr::pluck, "team") %>%
        purrr::map_depth(3, purrr::pluck, 2, "team_stats", "stats") %>%
        purrr::map_depth(4, purrr::pluck, "stat") %>%
        purrr::map_depth(4, dplyr::bind_rows) %>%
        purrr::map_depth(3, dplyr::bind_rows) %>%
        purrr::map_depth(3, tidyr::pivot_wider,
                         id_cols = stat_id,
                         names_from = stat_id,
                         values_from = value,
                         names_glue = "stat_id_{ stat_id }") %>%
        purrr::map_depth(3, purrr::map_if, purrr::negate(is.character), as.character) %>%
        purrr::map_depth(2, ~purrr::set_names(., nm = seq_along(.))) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows()


    #..................TEAM TOTAL WEEK POINTS DATA...................


    scoreboard_list$team_data$week_points <-
        scoreboard_list$inital_data %>%
        purrr::map_depth(2, purrr::pluck, "0", "teams") %>%
        purrr::map_depth(2, purrr::keep, purrr::is_list) %>%
        purrr::map_depth(3, purrr::pluck, "team") %>%
        purrr::map_depth(3, purrr::pluck, 2, "team_points") %>%
        purrr::map_depth(3, magrittr::extract, "total") %>%
        purrr::map_depth(3, purrr::map_if, purrr::negate(is.character), as.character) %>%
        purrr::map_depth(3, dplyr::bind_cols) %>%
        purrr::map_depth(2, ~purrr::set_names(., nm = seq_along(.))) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows()


    #..................TEAM WEEK GAME NUMBERS DATA...................


    scoreboard_list$team_data$game_counts <-
        scoreboard_list$inital_data %>%
        purrr::map_depth(2, purrr::pluck, "0", "teams") %>%
        purrr::map_depth(2, purrr::keep, purrr::is_list) %>%
        purrr::map_depth(3, purrr::pluck, "team") %>%
        purrr::map_depth(3, purrr::pluck, 2, "team_remaining_games", "total") %>%
        purrr::map_depth(3, dplyr::bind_cols) %>%
        purrr::map_depth(2, ~purrr::set_names(., nm = seq_along(.))) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows()


    #....................dplyr::bind AND JOIN LIST DATA.....................


    df <-
        scoreboard_list$matchup_meta %>%
        dplyr::right_join(
            dplyr::bind_cols(scoreboard_list$team_data), by = c("week", "matchup_id")
        ) %>%
        dplyr::left_join(scoreboard_list$stat_winners, by = c("week", "matchup_id"))


    #.............................RETURN.............................


    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                data = scoreboard_list,
                matchup_data = df
            ),
            class = "yahoo_fantasy_api")


    return(data_list)

}

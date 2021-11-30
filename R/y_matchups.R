#' Get matchup stats and results data from Yahoo! Fantasy API
#'
#' y_matchups() gets matchup stats and results for a given league and week.
#' It returns a list with the meta data as well as tibble containing the matchups data
#' in the element `matchups_data`.
#'
#' If the week argument is set to a fantasy week that has yet to occur the stats will all
#' show a value of 1 which the api returns as boolean value for is_tied.
#'
#' @param team_id as a string in the form "000.l.0000.t.00".  Team id can be found with y_teams().
#' @param token_name assigned object name used when creating token with y_create_token().
#' @param week the week of fantasy season to return. Default NULL will return all weeks of season.
#' @param debug Print uri and page counts to console as functions runs.  Useful for debugging.
#'
#' @return a tibble if debug = FALSE or a list if debug = TRUE
#' @export
y_matchups <-
    function(team_id = NULL, token_name = NULL, week = NULL, debug = FALSE) {

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        resource <- "team"
        subresource <- "matchups"
        api_token <- token_name

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        .team_id_check(team_id)
        .token_check(token_name, api_token, name = .GlobalEnv)
        stopifnot(is.null(week) | is.numeric(week))

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2", resource, team_id, subresource, sep = "/"),
                param = glue::glue("weeks={week}"),
                query = "format=json"
            )

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        r <-
            .y_get_response(uri, api_token)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        r_parsed <-
            .y_parse_response(r, "fantasy_content", "team")

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  EMPTY LIST                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        matchup_list <-
            list(
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

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        #........................MATCHUP META DATA.......................


        matchup_list$matchup_meta <-
            r_parsed %>%
            purrr::pluck(2, "matchups") %>%
            purrr::map(purrr::pluck, "matchup") %>%
            purrr::map(purrr::keep, purrr::negate(purrr::is_list)) %>%
            purrr::map(purrr::map_if, purrr::negate(is.character), as.character) %>%
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::map(dplyr::bind_cols) %>%
            dplyr::bind_rows(.id = "matchup_num")



        #..........................STAT WINNERS..........................


        matchup_list$stat_winners <-
            r_parsed %>%
            purrr::pluck(2, "matchups") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::map(purrr::pluck, "matchup", "stat_winners") %>%
            purrr::map_depth(2, purrr::pluck, "stat_winner") %>%
            purrr::map_depth(2,
                             purrr::map_if,
                             purrr::negate(is.character),
                             as.character) %>%
            purrr::map_depth(2, dplyr::bind_rows) %>%
            purrr::map(dplyr::bind_rows) %>%
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::map_if(
                purrr::negate(purrr::is_empty),
                tidyr::pivot_wider,
                id_cols = stat_id,
                names_from = stat_id,
                values_from = 2,
                names_glue = "stat_id_{ stat_id }_winner"
            ) %>%
            dplyr::bind_rows(.id = "matchup_num")


        #.........................TEAM META DATA.........................


        matchup_list$team_data$meta <-
            r_parsed %>%
            purrr::pluck(2, "matchups") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::map(purrr::pluck, "matchup", "0") %>%
            purrr::map(purrr::pluck, "teams") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::map_depth(2, purrr::pluck, "team", 1) %>%
            purrr::map_depth(2, magrittr::extract, 1:3) %>%
            purrr::map_depth(2, dplyr::bind_cols) %>%
            purrr::map_depth(2,
                             purrr::map_if,
                             purrr::negate(is.character),
                             as.character) %>%
            purrr::map(dplyr::bind_rows) %>%
            purrr::set_names(nm = seq_along(.)) %>%
            dplyr::bind_rows(.id = "matchup_num")


        #...........................TEAM STATS...........................


        matchup_list$team_data$stats <-
            r_parsed %>%
            purrr::pluck(2, "matchups") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::map(purrr::pluck, "matchup", "0") %>%
            purrr::map(purrr::pluck, "teams") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::map_depth(2, purrr::pluck, "team", 2, "team_stats", "stats") %>%
            purrr::map_depth(3, purrr::pluck, "stat") %>%
            purrr::map_depth(2, dplyr::bind_rows) %>%
            purrr::map_depth(
                2,
                tidyr::pivot_wider,
                id_cols = stat_id,
                names_from = stat_id,
                values_from = value,
                names_glue = "stat_id_{ stat_id }"
            ) %>%
            purrr::map_depth(2,
                             purrr::map_if,
                             purrr::negate(is.character),
                             as.character) %>%
            purrr::map(dplyr::bind_rows) %>%
            purrr::set_names(nm = seq_along(.)) %>%
            dplyr::bind_rows() %>%
            dplyr::mutate(dplyr::across(.cols = dplyr::everything(), as.numeric))


        #........................TOTAL WEEK POINTS.......................


        matchup_list$team_data$week_points <-
            r_parsed %>%
            purrr::pluck(2, "matchups") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::map(purrr::pluck, "matchup", "0") %>%
            purrr::map(purrr::pluck, "teams") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::map_depth(2, purrr::pluck, "team", 2, "team_points") %>%
            purrr::map_depth(2, magrittr::extract, "total") %>%
            purrr::map_depth(2,
                             purrr::map_if,
                             purrr::negate(is.character),
                             as.character) %>%
            purrr::map_depth(2, dplyr::bind_cols) %>%
            purrr::map(dplyr::bind_rows) %>%
            purrr::set_names(nm = seq_along(.)) %>%
            dplyr::bind_rows()


        #......................GAMES PLAYED COUNTS.......................


        matchup_list$team_data$game_counts <-
            r_parsed %>%
            purrr::pluck(2, "matchups") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::map(purrr::pluck, "matchup", "0") %>%
            purrr::map(purrr::pluck, "teams") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::map_depth(2, purrr::pluck, "team", 2, "team_remaining_games", "total") %>%
            purrr::map_depth(2, dplyr::bind_cols) %>%
            purrr::map(dplyr::bind_rows) %>%
            purrr::set_names(nm = seq_along(.)) %>%
            dplyr::bind_rows()

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        df <-
            dplyr::left_join(
                purrr::reduce(matchup_list[["team_data"]], dplyr::bind_cols),
                matchup_list[["stat_winners"]],
                by = "matchup_num"
            ) %>%
            dplyr::left_join(matchup_list[["matchup_meta"]], ., by = "matchup_num")

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    RETURN                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        data_list <-
            structure(list(
                content = r_parsed,
                uri = uri,
                data_list = matchup_list,
                df = df
            ),
            class = "yahoo_fantasy_api")

        if(debug){return(data_list)}

        return(df)

    }

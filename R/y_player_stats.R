#' Get player stats
#'
#' @param players player keys.  Keys are in the form xxx.p.xxxx and found using
#' `y_players()`, `y_player_slate()` or `y_rosters()`
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param game_date Date of fantasy season in form YYYY-MM-DD to return.
#' Default is NULL which will return aggregate stats for current season.  Accepts a vector of dates.
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a tibble or list
#' @export
y_player_stats <-
    function(players = NULL, token_name = NULL, game_date = NULL, debug = FALSE) {

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        api_token <- token_name
        resource <- "players"
        subresource <- "stats"
        players <- glue::glue_collapse(players, sep = ",")


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        .player_id_check(players)
        .token_check(token_name, api_token, name = .GlobalEnv)
        if(!is.null(game_date)){
            .date_check(glue::glue_collapse(game_date, sep = ","))
            }


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        uri <-
            httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = glue::glue("fantasy/v2","players;player_keys={players}", subresource, .sep = "/"),
            param = glue::glue("type=date", "date={game_date}", .sep = ";"),
            query = "format=json"
        )


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        r <-
            purrr::map(uri, .y_get_response, api_token)


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        r_parsed <-
            purrr::map(r, .y_parse_response, "fantasy_content", "players")


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        if (!debug) {

            #........................preprocess list.........................


            preprocess <-
                r_parsed %>%
                purrr::map(purrr::keep, purrr::is_list) %>%
                purrr::map(purrr::compact) %>%
                purrr::flatten()


            #..........................player_info...........................


            player_info <-
                preprocess %>%
                purrr::map_df(.player_meta_func, "player", 1, .id = "week_day")


            #..........................player_stats..........................


            stats <-
                preprocess %>%
                purrr::map_df(.player_stats_parse)


            #...............................DF...............................


            df <-
                dplyr::bind_cols(player_info, stats)

            return(df)

        }


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                DEBUG RETURN                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        data_list <-
            structure(list(
                response = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")

        return(data_list)


    }

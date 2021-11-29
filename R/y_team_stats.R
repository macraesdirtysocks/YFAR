#' Get team level stats for scoring categories
#'
#' returns team or league stats depending on what id (league or team) is supplied to `id`
#'
#' @param id league id or team id as a string in the form "000.l.0000" or "000.l.0000.t.0".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with `y_create_token()`.
#' @param week week of fantasy season to return.  Accepts 3 arguments `current`, `NULL`, `a number`.
#' `current` by default returns current week, null returns aggregated season stats, number returns stats for that week.
#'
#' @return a list
#' @export
y_team_stats <- function(id = NULL, token_name = NULL, week = "current") {

    api_token <- token_name

    .token_check(token_name, api_token, name = .GlobalEnv)
    stopifnot(is.numeric(week) | is.null(week) | week == "current")

    # the if statement below accounts for the response depending on the arguments
    # supplied. if team_id is supplied mapping is not necessary but if a league_id
    # is supplied mapping is necessary because you get a list for each team.

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                          LEAGUE RESOURCE TEAM STATS                      ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      if                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # tests if id is a league id.

    if (stringr::str_detect(id, pattern = "[:digit:]*(\\.l\\.[:digit:]*)$") == TRUE) {

        #.......................RESOURCE VARIABLES.......................

        resource <- "league"
        subresource1 <- "teams"
        subresource2 <- "stats"
        league_id <- id

        #..............................URI...............................

        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste(
                    "fantasy/v2",
                    resource,
                    league_id,
                    subresource1,
                    subresource2,
                    sep = "/"
                ),
                param = glue::glue("type=week;week={week}"),
                query = "format=json"
            )

        #............................RESPONSE............................

        r <-
            .y_get_response(uri, api_token)

        r_parsed <-
            .y_parse_response(r, "fantasy_content", resource)

        #......................INITIALIZE EMPTY LIST.....................


        team_stats_list <-
            list(
                team_meta = NULL,
                team_points = NULL,
                team_stats = NULL,
                games_played = NULL
            )

        #............................TEAM META...........................


        team_stats_list$team_meta <-
            r_parsed %>%
            purrr::pluck(2, "teams") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::compact() %>%
            purrr::map(purrr::pluck, "team", 1) %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::map(purrr::compact) %>%
            purrr::map(`[`, 1:3) %>%
            purrr::map(dplyr::bind_cols) %>%
            purrr::map_df(dplyr::bind_rows)


        #...........................TEAM STATS...........................


        team_stats_list$team_stats <-
            r_parsed %>%
            purrr::pluck(2, "teams") %>%
            purrr::map(purrr::pluck, "team", 2, "team_stats", "stats") %>%
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::map_depth(2, purrr::pluck, "stat") %>%
            purrr::map_depth(2, dplyr::bind_rows) %>%
            purrr::map_df(dplyr::bind_rows, .id = "team_id") %>%
            tidyr::pivot_wider(
                id_cols = c(team_id, stat_id),
                names_from = stat_id,
                values_from = value,
                names_prefix = "stat_id_"
            )


        #..........................TEAM POINTS...........................


        team_stats_list$team_points <-
            r_parsed %>%
            purrr::pluck(2, "teams") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::compact() %>%
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::map(purrr::pluck, "team", 2, "team_points") %>%
            purrr::map(., ~purrr::set_names(., nm = paste("points", names(.), sep = "_"))) %>%
            purrr::map_df(dplyr::bind_cols, .id = "team_id")


        #..........................GAMES PLAYED..........................


        team_stats_list$games_played <-
            r_parsed %>%
            purrr::pluck(2, "teams") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::compact() %>%
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::map(purrr::pluck, "team", 2, "team_remaining_games", "total") %>%
            purrr::map_df(dplyr::bind_cols, .id = "team_id")


        #...............................DF...............................


        df <- team_stats_list %>%
            purrr::keep(purrr::is_list) %>%
            purrr::reduce(dplyr::left_join, by = "team_id")

        #.............................RETURN.............................

        data_list <-
            structure(
                list(
                    content = r_parsed,
                    uri = uri,
                    data_parsed = team_stats_list,
                    df = df
                ),
                class = "yahoo_fantasy_api"
            )

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##  ~ league stats return  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        return(data_list)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                          TEAM RESOURCE TEAM STATS                        ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # tests if supplied id is a team_id
        # mapping here is not required because team_id will always return a single list
        # whether for a single week or the season.

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   else if                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    } else if (stringr::str_detect(id, pattern = "[:digit:]*\\.l\\.[:digit:]*(\\.t\\.[:digit:])$") == TRUE) {

        resource <- "team"
        subresource <- "stats"
        team_id <- id

        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2", resource, team_id, subresource, sep = "/"),
                param = glue::glue("type=week;week={week}"),
                query = "format=json"
            )

        r <-
            .y_get_response(uri, api_token)

        r_parsed <-
            .y_parse_response(r, "fantasy_content", resource)

        team_stats_list <-
            list(
                team_meta = NULL,
                team_points = NULL,
                team_stats = NULL,
                games_played = NULL
            )

        #............................TEAM META...........................

        team_stats_list$team_meta <-
            r_parsed %>%
            purrr::pluck(1) %>%
            purrr::keep(purrr::is_list) %>%
            purrr::compact() %>%
            .[1:3] %>%
            purrr::map(dplyr::bind_rows) %>%
            dplyr::bind_cols()


        #...........................TEAM STATS...........................


        team_stats_list$team_stats <-
            r_parsed %>%
            purrr::pluck(2, "team_stats", "stats") %>%
            purrr::map(purrr::pluck, "stat") %>%
            purrr::map_df(dplyr::bind_rows) %>%
            tidyr::pivot_wider(
                id_cols = stat_id,
                names_from = stat_id,
                values_from = value,
                names_prefix = "stat_id_"
            )


        #..........................TEAM POINTS...........................


        team_stats_list$team_points <-
            r_parsed %>%
            purrr::pluck(2, "team_points") %>%
            purrr::set_names(nm = paste("points", names(.), sep = "_")) %>%
            dplyr::bind_cols()


        #..........................GAMES PLAYED..........................


        team_stats_list$games_played <-
            r_parsed %>%
            purrr::pluck(2, "team_remaining_games", "total") %>%
            dplyr::bind_cols()


        #...............................DF...............................


        df <- team_stats_list %>%
            purrr::compact() %>% # purrr::compact is needed here because week = NULL will not return games_played
            purrr::reduce(dplyr::bind_cols)

        #.............................RETURN.............................

        data_list <-
            structure(
                list(
                    content = r_parsed,
                    uri = uri,
                    data_parsed = team_stats_list,
                    df = df
                ),
                class = "yahoo_fantasy_api"
            )

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##  ~ team stats return  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~

        return(data_list)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    else                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    } else {

        stop(message("please supply a league_id or team_id"))

    }
}





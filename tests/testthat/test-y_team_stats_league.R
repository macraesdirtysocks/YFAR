context("Get league team stats")
library(YFAR)

with_mock_api({
    testthat::test_that("request returns valid response and is parsed to a tibble", {

        #..........................uri arguments.........................

        resource <- "league"
        subresource1 <- "teams"
        subresource2 <- "stats"
        league_id <- "411.l.1239"
        week <- 1

        #............................Build uri...........................

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

        # testthat uri is built as expected
        testthat::expect_identical(
            uri,
            "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/teams/stats;type=week;week=1?format=json"
        )

        # GET
        r <- .y_get_response(uri)

        # test response uri matches desired uri
        testthat::expect_identical(r$url, uri)

        # test that r is a response class
        testthat::expect_s3_class(r, class = "response")

        # test that response is json format
        testthat::expect_identical(httr::http_type(r), "application/json")

        # test that response is not an error
        testthat::expect_identical(httr::status_code(r), 200L)

        # Content
        r_parsed <-
            .y_parse_response(r, "fantasy_content", resource)


        #..........................Parse content.........................

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
            purrr::keep(purrr::is_list) %>%
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::map_depth(2, purrr::pluck, "stat") %>%
            purrr::map_depth(2, purrr::flatten_df) %>%
            purrr::map(dplyr::bind_rows) %>%
            # # convert stat id numbers to display name i.e. stat 1 = G
            purrr::map(dplyr::left_join,
                       .yahoo_hockey_stat_categories(),
                       by = "stat_id") %>%
            purrr::map(dplyr::select, display_name, value) %>%
            purrr::map(
                tidyr::pivot_wider,
                id_cols = display_name,
                names_from = display_name,
                values_from = value
            ) %>%
            dplyr::bind_rows(.id = "team_id")


        #..........................TEAM POINTS...........................


        team_stats_list$team_points <-
            r_parsed %>%
            purrr::pluck(2, "teams") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::compact() %>%
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::map(purrr::pluck, "team", 2, "team_points") %>%
            purrr::map(., ~ purrr::set_names(., nm = paste("points", names(.), sep = "_"))) %>%
            purrr::map_df(dplyr::bind_cols, .id = "team_id")


        #..........................GAMES PLAYED..........................


        team_stats_list$games_played <-
            r_parsed %>%
            purrr::pluck(2, "teams") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::compact() %>%
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::map(purrr::pluck,
                       "team",
                       2,
                       "team_remaining_games",
                       "total") %>%
            purrr::map_df(dplyr::bind_cols, .id = "team_id")


        #...............................DF...............................


        df <- team_stats_list %>%
            purrr::keep(purrr::is_list) %>%
            purrr::reduce(dplyr::left_join, by = "team_id")

        # test that df length == 12
        expect_true(nrow(df) == 12)

        # expected colnames
        x <-
            c("team_key", "team_id", "name", "points_coverage_type", "points_week",
              "points_total", "g", "a", "x", "ppp", "sog", "hit", "w", "ga",
              "gaa", "sv", "sa", "sv_percent", "sho", "remaining_games", "live_games",
              "completed_games")

        # test that colnames of the df match expected
        testthat::expect_named(df,
                               x,
                               ignore.order = TRUE,
                               ignore.case = TRUE)


    })
})

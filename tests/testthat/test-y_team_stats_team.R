library(YFAR)

with_mock_api({
    testthat::test_that("request returns valid response and is parsed to a tibble", {

        # uri components
        resource <- "team"
        subresource <- "stats"
        team_id <- "411.l.1239.t.1"
        week <- 5

        # Build uri
        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2", resource, team_id, subresource, sep = "/"),
                param = glue::glue("type=week;week={week}"),
                query = "format=json"
            )

        # testthat uri is built as expected
        testthat::expect_identical(
            uri,
            "https://fantasysports.yahooapis.com/fantasy/v2/team/411.l.1239.t.1/stats;type=week;week=5?format=json"
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
            purrr::map_df(purrr::flatten_df) %>%
            dplyr::left_join(.yahoo_hockey_stat_categories(), by = "stat_id") %>%
            dplyr::select(display_name, value) %>%
            tidyr::pivot_wider(id_cols = display_name,
                               names_from = display_name,
                               values_from = value)



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

        # test that df length == 1
        expect_true(nrow(df) == 1)

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

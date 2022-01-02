library(YFAR)

with_mock_api({

    testthat::test_that("request returns valid response and is parsed to a tibble",{


        uri <- "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/scoreboard?format=json"

        # GET response
        r <- .y_get_response(uri = uri)

        # test response uri matches desired uri
        testthat::expect_identical(r$url, uri)

        # test that r is a response class
        testthat::expect_s3_class(r, class = "response")

        # test that response is json format
        testthat::expect_identical(httr::http_type(r), "application/json")

        # test that response is not an error
        testthat::expect_false(httr::http_error(r))

        # test that the response contains the matchup subresource
        testthat::expect_identical(
            names(.y_parse_response(r, "fantasy_content", "league", 2)), "scoreboard")

        # get content from response
        r_parsed <- .y_parse_response(r)

        # test that response contains 6 matchups
        testthat::expect_gte(
            purrr::pluck(r_parsed, "fantasy_content", "league", 2, "scoreboard", "0", "matchups","count"), 6)

        preprocess <-
            r_parsed %>%
            purrr::pluck("fantasy_content", "league", 2, "scoreboard") %>%
            purrr::map(purrr::pluck, "matchups") %>%
            purrr::map_depth(2, purrr::pluck, "matchup") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::compact() %>%
            purrr::flatten()

        # test that the preprocess is length 1 after plucking out the matchup resource
        testthat::expect_gte(length(preprocess), 6)

        df <-
            purrr::map_df(preprocess, .matchup_parse_fn, .id = "matchup")

        # test that df is a tibble
        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        # expected colnames
        x <-
            c("matchup", "week", "week_start", "week_end", "status", "is_playoffs",
              "is_consolation", "g", "a", "x", "ppp", "sog", "hit", "w", "gaa",
              "sv_percent", "sho", "team_key", "team_id", "name", "coverage_type",
              "count_g", "count_a", "count_x", "count_ppp", "count_sog", "count_hit",
              "count_w", "count_ga", "count_gaa", "count_sv", "count_sa", "count_sv_percent",
              "count_sho", "total", "remaining_games", "live_games", "completed_games"
            )

        # test that colnames of the df match expected
        testthat::expect_named(df, x, ignore.order = TRUE, ignore.case = TRUE)

    })
})


# test y_scoreboard with multiple weeks in week argument

with_mock_api({

    testthat::test_that("request returns valid response and is parsed to a tibble",{

        uri <- c("https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/scoreboard;week=4?format=json",
                 "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/scoreboard;week=5?format=json")

        r <- purrr::map(uri, .y_get_response)

        # test response uri matches desired uri
        r %>%
            purrr::map_chr(purrr::pluck, "url") %>%
            testthat::expect_identical(uri)

        # test that r is a response class
        purrr::map(r, ~testthat::expect_s3_class(.x, class = "response"))


        # test that response is json format
        purrr::map(r, ~testthat::expect_identical(httr::http_type(.x), "application/json"))

        # test that response is not an error
        purrr::map(r, ~testthat::expect_false(httr::http_error(.x)))

        # test that the response contains the matchup subresource
        purrr::map(r, ~testthat::expect_identical(
            names(.y_parse_response(.x, "fantasy_content", "league", 2)), "scoreboard"))

        # get content from response
        r_parsed <- purrr::map(r, .y_parse_response)

        # test that response contains 6 matchups
        purrr::map(r_parsed, ~testthat::expect_gte(
            purrr::pluck(.x, "fantasy_content", "league", 2, "scoreboard", "0", "matchups","count"), 6))

        preprocess <-
            r_parsed %>%
            purrr::map(purrr::pluck, "fantasy_content", "league", 2, "scoreboard", "0", "matchups") %>%
            purrr::map_depth(2, purrr::pluck, "matchup") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::compact() %>%
            purrr::flatten()

        # test that the preprocess is length 1 after plucking out the matchup resource
        testthat::expect_length(preprocess, 12)

        df <-
            purrr::map_df(preprocess, .matchup_parse_fn, .id = "matchup")

        # test that df is a tibble
        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        # expected colnames
        x <-
            c("matchup", "week", "week_start", "week_end", "status", "is_playoffs",
              "is_consolation", "is_tied", "winner_team_key", "team_key", "team_id",
              "name", "coverage_type", "count_g", "count_a", "count_x", "count_ppp",
              "count_sog", "count_hit", "count_w", "count_ga", "count_gaa",
              "count_sv", "count_sa", "count_sv_percent", "count_sho", "total",
              "remaining_games", "live_games", "completed_games", "g", "a",
              "x", "ppp", "sog", "hit", "w", "gaa", "sv_percent", "sho")

        # test that colnames of the df match expected
        testthat::expect_named(df, x, ignore.order = TRUE, ignore.case = TRUE)

})
})

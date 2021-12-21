context("Get game data")
library(YFAR)

with_mock_api({

    testthat::test_that("request returns valid response and is parsed to a tibble",{

        uri <- "https://fantasysports.yahooapis.com/fantasy/v2/team/411.l.1239.t.1/matchups;weeks=1?format=json"
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
            names(.y_parse_response(r, "fantasy_content", "team", 2)), "matchups")

        # get content from response
        r_parsed <- .y_parse_response(r)

        # test that response contains only 1 matchup subresource because we supplied a team id and week argument
        testthat::expect_gte(
            purrr::pluck(r_parsed, "fantasy_content", "team", 2, "matchups", "count"), 1)

        # test that the team resource is length 2
        testthat::expect_gte(length(purrr::pluck(r_parsed, "fantasy_content", "team")), 2)

        preprocess <-
            r_parsed %>%
            purrr::map(purrr::pluck, "team", 2, "matchups") %>%
            purrr::map_depth(2, purrr::pluck, "matchup") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::compact() %>%
            purrr::flatten()

        preprocess <-
            preprocess[purrr::map_lgl(preprocess, purrr::negate(~purrr::has_element(.x, "preevent")))]


        # test that the preprocess is length 1 after plucking out the matchup resource
        testthat::expect_gte(length(preprocess), 1)

        df <-
            purrr::map_df(preprocess, .matchup_parse_fn)

        # test that df is a tibble
        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        # expected colnames
        x <- c("week", "week_start", "week_end", "status", "is_playoffs",
               "is_consolation", "is_tied", "winner_team_key", "g", "a", "x",
               "ppp", "sog", "hit", "w", "gaa", "sv_percent", "sho", "team_key",
               "team_id", "name", "point_total", "remaining_games", "live_games",
               "completed_games", "count_g", "count_a", "count_x", "count_ppp",
               "count_sog", "count_hit", "count_w", "count_ga", "count_gaa",
               "count_sv", "count_sa", "count_sv_percent", "count_sho")

        # test that colnames of the df match expected
        testthat::expect_named(df, x, ignore.order = TRUE, ignore.case = TRUE)

    })
})

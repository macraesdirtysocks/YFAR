context("Get draft results")
library(YFAR)

with_mock_api({

    testthat::test_that("request returns valid response and is parsed to a tibble",{

        uri <- "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/teams/draftresults?format=json"
        r <- .y_get_response(uri = uri)
        testthat::expect_identical(r$url, uri)
        testthat::expect_s3_class(r, class = "response")
        testthat::expect_identical(httr::http_type(r), "application/json")
        testthat::expect_false(httr::http_error(r))
        testthat::expect_identical(
            names(.y_parse_response(r, "fantasy_content", "league", 2)), "teams")

        r_parsed <- .y_parse_response(r, "fantasy_content", "league", 2, "teams")
        testthat::expect_gte(length(r_parsed), 4)

        preprocess <-
            r_parsed %>%
            purrr::keep(purrr::is_list) %>%
            purrr::map(purrr::pluck, "team", 2, "draft_results") %>%
            purrr::map(purrr::keep, purrr::is_list)
        df <- purrr::map_df(preprocess, purrr::flatten_df)
        testthat::expect_equal(tibble::is_tibble(df), TRUE)
        testthat::expect_equal(ncol(df), 4)

        x <- c("pick", "round", "team_key", "player_key")

        testthat::expect_named(df, x, ignore.order = TRUE, ignore.case = TRUE)

    })
})

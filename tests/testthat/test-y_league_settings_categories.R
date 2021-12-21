context("Get category league settings")
library(YFAR)

with_mock_api({
    testthat::test_that("request returns valid response and is parsed to a tibble", {
        uri <-
            "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/settings?format=json"
        r <- .y_get_response(uri = uri)
        testthat::expect_identical(r$url, uri)
        testthat::expect_s3_class(r, class = "response")
        testthat::expect_identical(httr::http_type(r), "application/json")
        testthat::expect_false(httr::http_error(r))
        testthat::expect_identical(names(.y_parse_response(r, "fantasy_content", "league", 2)), "settings")

        r_parsed <-
            .y_parse_response(r, "fantasy_content", "league")

        testthat::expect_gte(length(r_parsed), 2)

        testthat::expect_equal(purrr::pluck(r_parsed, 1, "scoring_type"), "head")

        df <- .category_league_settings(r_parsed)

        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        testthat::expect_equal(dim(df), c(4, 2))

        x <- c("info", "data")

        testthat::expect_named(df,
                               x,
                               ignore.order = TRUE,
                               ignore.case = TRUE)

    })
})

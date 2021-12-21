context("Get fanatasy weeks")
library(YFAR)

with_mock_api({

    testthat::test_that("request returns valid response and is parsed to a tibble",{

        uri <- "https://fantasysports.yahooapis.com/fantasy/v2/game/411/game_weeks?format=json"
        r <- .y_get_response(uri = uri)
        testthat::expect_identical(r$url, uri)
        testthat::expect_s3_class(r, class = "response")
        testthat::expect_identical(httr::http_type(r), "application/json")
        testthat::expect_false(httr::http_error(r))
        testthat::expect_identical(
            names(.y_parse_response(r, "fantasy_content")), "game")

        r_parsed <- .y_parse_response(r, "fantasy_content", "game")

        testthat::expect_equal(length(r_parsed), 2)

        df <-
            r_parsed %>%
            purrr::pluck(1) %>%
            dplyr::bind_rows()

        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        x <- c("game_key",
               "game_id", "name", "code", "type", "url", "season", "is_registration_over",
               "is_game_over", "is_offseason")

        testthat::expect_named(df, x, ignore.order = TRUE, ignore.case = TRUE)

    })
})

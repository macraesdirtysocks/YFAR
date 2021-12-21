context("Get league standings")
library(YFAR)

with_mock_api({
    testthat::test_that("request returns valid response and is parsed to a tibble",{

        uri <-
            "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/standings?format=json"
        r <- .y_get_response(uri = uri)

        # test response uri matches desired uri
        testthat::expect_identical(r$url, uri)

        # test that r is a response class
        testthat::expect_s3_class(r, class = "response")

        # test that response is json format
        testthat::expect_identical(httr::http_type(r), "application/json")

        # test that response is not an error
        testthat::expect_identical(httr::status_code(r), 200L)

        # get content
        r_parsed <-
            .y_parse_response(r, "fantasy_content", "league", 2, "standings", 1, "teams") %>%
            purrr::map(purrr::pluck, "team") %>%
            purrr::compact()

        # parse function
        standings_parse <- function(x){

            team_meta <-
                x %>%
                purrr::pluck(1) %>%
                magrittr::extract(1:3) %>%
                purrr::flatten_df()

            team_stats <-
                x %>%
                purrr::pluck(2, "team_stats", "stats") %>%
                purrr::flatten_df() %>%
                dplyr::left_join(., .yahoo_hockey_stat_categories(), by = "stat_id") %>%
                dplyr::select("display_name", "value") %>%
                tidyr::pivot_wider(
                    id_cols = display_name,
                    names_from = display_name,
                    values_from = value)

            team_points <-
                x %>%
                purrr::pluck(2, "team_points") %>%
                purrr::flatten_df()

            standings <-
                x %>%
                purrr::pluck(3, "team_standings") %>%
                purrr::keep(purrr::negate(purrr::is_list)) %>%
                dplyr::bind_cols()

            outcomes <-
                x %>%
                purrr::pluck(3, "team_standings", "outcome_totals") %>%
                dplyr::bind_cols()

            df <- dplyr::bind_cols(team_meta, team_points, standings, outcomes, team_stats)

        }

        df <-
            purrr::map_df(r_parsed, standings_parse)


        # test that df is a tibble
        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        # expected colnames
        x <-
            c("team_key", "team_id", "name", "coverage_type", "season", "total",
               "rank", "playoff_seed", "wins", "losses", "ties", "percentage",
               "g", "a", "x", "ppp", "sog", "hit", "w", "ga", "gaa", "sv", "sa",
               "sv_percent", "sho")


        # test that colnames of the df match expected
        testthat::expect_named(df,
                               x,
                               ignore.order = TRUE,
                               ignore.case = TRUE)




    })
})

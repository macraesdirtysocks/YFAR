context("Get player slate")
library(YFAR)

with_mock_api({

    testthat::test_that("request returns valid response and is parsed to a tibble",{

        uri <- "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/players;start=0;count=25?format=json"
        r <- .y_get_response(uri = uri)
        testthat::expect_identical(r$url, uri)
        testthat::expect_s3_class(r, class = "response")
        testthat::expect_identical(httr::http_type(r), "application/json")
        testthat::expect_false(httr::http_error(r))
        testthat::expect_identical(httr::status_code(r), 200L)
        testthat::expect_identical(
            names(.y_parse_response(r, "fantasy_content", "league", 2)), "players")

        r_parsed <- .y_parse_response(r, "fantasy_content", "league", 2, "players")

        testthat::expect_gte(length(r_parsed), 25)

        df <- purrr::map_df(r_parsed, .player_parse_fn)

        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        x <- c("player_key", "player_id", "name_full", "name_first", "name_last",
               "name_ascii_first", "name_ascii_last", "editorial_player_key",
               "editorial_team_key", "editorial_team_full_name", "editorial_team_abbr",
               "uniform_number", "display_position", "headshot_url", "headshot_size",
               "image_url", "is_undroppable", "position_type", "primary_position",
               "eligible_positions_position", "eligible_positions_position_2",
               "status", "status_full", "injury_note", "on_disabled_list", "eligible_positions_position_3",
               "has_player_notes", "player_notes_last_timestamp", "has_recent_player_notes"
        )

        testthat::expect_named(df, x, ignore.order = TRUE, ignore.case = TRUE)

    })
})


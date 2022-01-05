library(YFAR)

with_mock_api({
    testthat::test_that("request returns valid response and is parsed to a tibble",{

        uri <-
            .uri_gen_func(
                start = 0,
                number_of_players = 25,
                resp_len = 25,
                league_id = "411.l.1239",
                resource = "league",
                subresource = "players"
            )


        # testthat uri is built as expected
        testthat::expect_identical(uri, "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/players;start=0;count=25?format=json")

        r <- .y_get_response(uri)

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
            .y_parse_response(r, "fantasy_content", "league", 2, "players")

        # parse function
        df <- purrr::map_df(r_parsed, .player_meta_func, "player", 1)


        # test that df is a tibble
        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        # expected colnames
        x <-
            c("player_key", "player_id", "name_full", "name_first", "name_last",
              "name_ascii_first", "name_ascii_last", "editorial_player_key",
              "editorial_team_key", "editorial_team_full_name", "editorial_team_abbr",
              "uniform_number", "display_position", "headshot_url", "headshot_size",
              "image_url", "is_undroppable", "position_type", "primary_position",
              "eligible_positions_position", "eligible_positions_position_2",
              "has_player_notes", "player_notes_last_timestamp", "status",
              "status_full", "injury_note", "on_disabled_list", "has_recent_player_notes",
              "eligible_positions_position_3")


        # test that colnames of the df match expected
        testthat::expect_named(df,
                               x,
                               ignore.order = TRUE,
                               ignore.case = TRUE)




    })
})


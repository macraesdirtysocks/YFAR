library(YFAR)

with_mock_api({
    testthat::test_that("request returns valid response and is parsed to a tibble",{

        uri <-
            .uri_gen_func(
                number_of_players = 10,
                resp_len = 25,
                league_id = "411.l.1239",
                resource = "league",
                subresource = "players",
                sort = 1L,
                status = "ALL",
                start = 0
            )


        # testthat uri is built as expected
        testthat::expect_identical(uri, "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/players;sort=1;status=ALL;start=0;count=10?format=json")

        r <-
            .y_get_response(uri)

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
        df <-
            r_parsed %>%
            purrr::flatten() %>%
            purrr::map(.player_parse_fn) %>%
            # add in rank column which is not included in the response
            purrr::set_names(nm = seq_along(.)) %>%
            purrr::imap(~purrr::prepend(.x, list("rank" = .y))) %>%
            purrr::map_df(dplyr::bind_cols)

        #....................remove duplicate players....................

        df <-
            df %>%
            dplyr::group_by(player_id) %>%
            dplyr::slice_head(n = 1) %>%
            dplyr::ungroup()


        # test that df is a tibble
        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        # expected colnames
        x <-
            c("rank", "player_key", "player_id", "name_full", "name_first",
              "name_last", "name_ascii_first", "name_ascii_last", "editorial_player_key",
              "editorial_team_key", "editorial_team_full_name", "editorial_team_abbr",
              "uniform_number", "display_position", "headshot_url", "headshot_size",
              "image_url", "is_undroppable", "position_type", "primary_position",
              "eligible_positions_position", "eligible_positions_position_2",
              "has_player_notes", "player_notes_last_timestamp", "has_recent_player_notes"
            )


        # test that colnames of the df match expected
        testthat::expect_named(df,
                               x,
                               ignore.order = TRUE,
                               ignore.case = TRUE)




    })
})


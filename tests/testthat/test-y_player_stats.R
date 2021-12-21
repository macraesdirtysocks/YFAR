context("Get player stats")
library(YFAR)

with_mock_api({
    testthat::test_that("request returns valid response and is parsed to a tibble",{

    uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6369/stats;type=date;date=2021-12-10?format=json"
    r <- purrr::map(uri, .y_get_response)

    # test response uri matches desired uri
    testthat::expect_identical(r[[1]][["url"]], uri)

    # test that r is a response class
    testthat::expect_s3_class(r[[1]], class = "response")

    # test that response is json format
    testthat::expect_identical(httr::http_type(r[[1]]), "application/json")

    # test that response is not an error
    testthat::expect_identical(httr::status_code(r[[1]]), 200L)

    r_parsed <-
        purrr::map(r, .y_parse_response, "fantasy_content", "players")


    #........................preprocess list.........................


    preprocess <-
        r_parsed %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(purrr::compact) %>%
        purrr::map_depth(2, purrr::pluck, "player")


    #..........................player_info...........................


    player_info <-
        preprocess %>%
        purrr::map_depth(2, purrr::pluck, 1) %>%
        purrr::map_df(purrr::map_df, .player_parse_fn, .id = "week_day")


    #..........................player_stats..........................


    stats <-
        preprocess %>%
        purrr::map_df(purrr::map_df, .player_stats_parse)


    df <-
        dplyr::bind_cols(player_info, stats)


    # test that df is a tibble
    testthat::expect_equal(tibble::is_tibble(df), TRUE)

    # expected colnames
    x <-
        c(
            "week_day",
            "player_key",
            "player_id",
            "name_full",
            "name_first",
            "name_last",
            "name_ascii_first",
            "name_ascii_last",
            "editorial_player_key",
            "editorial_team_key",
            "editorial_team_full_name",
            "editorial_team_abbr",
            "uniform_number",
            "display_position",
            "headshot_url",
            "headshot_size",
            "image_url",
            "is_undroppable",
            "position_type",
            "eligible_positions_position",
            "eligible_positions_position_2",
            "has_player_notes",
            "player_notes_last_timestamp",
            "gp",
            "gp_2",
            "g",
            "a",
            "p",
            "x",
            "pim",
            "ppg",
            "ppa",
            "ppp",
            "shg",
            "sha",
            "shp",
            "gwg",
            "gtg",
            "sog",
            "sh_percent",
            "fw",
            "fl",
            "hit",
            "blk"
        )

    # test that colnames of the df match expected
    testthat::expect_named(df,
                           x,
                           ignore.order = TRUE,
                           ignore.case = TRUE)




    })
})

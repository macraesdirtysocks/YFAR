library(YFAR)

testthat::test_that("uri is generated properly", {
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                                                            ~~
    ##                                URI FUNCTION                              ----
    ##                                                                            ~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    y_league_settings_uri_gen_test_fn <- function(league_key) {
        resource <- "leagues"
        subresource <- "settings"
        uri_out <- "league_keys="

        key <- .single_resource_key_check(league_key, .league_key_check)

        # Initial uri components
        uri_parsed <- structure(
            list(
                scheme = "https",
                hostname = "fantasysports.yahooapis.com/fantasy/v2",
                port = NULL,
                path = resource,
                query = list(format = "json"),
                params = NULL
            ),
            class = "url"
        )

        key_path <-
            .uri_path_packer(key)

        # Create paths using .uri_path_package(key) to pack player keys into groups of 25.
        uri_parsed$params <-
            stringr::str_c(uri_out, key_path, "/", subresource, sep = "")

        # Build uris.
        uri <- httr::build_url(uri_parsed)
    }


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                                                            ~~
    ##                                URI GEN TESTS                             ----
    ##                                                                            ~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    league_id_1 <- "411.l.1239"
    league_id_2 <- "411.l.1240"
    league_id_3 <- "321.l.35439"

    # Test for a single league key.
    expect_equal(
        y_league_settings_uri_gen_test_fn(league_id_1),
        stringr::str_c(
            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=",
            league_id_1,
            "/settings?format=json"
        )
    )

    # Test for a single league key.
    expect_equal(
        y_league_settings_uri_gen_test_fn(league_id_3),
        stringr::str_c(
            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=",
            league_id_3,
            "/settings?format=json"
        )
    )

    # Test for 2 league keys.
    expect_equal(
        y_league_settings_uri_gen_test_fn(c(league_id_1, league_id_2)),
        stringr::str_c(
            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=",
            stringr::str_flatten(c(league_id_1, league_id_2), collapse = ","),
            "/settings?format=json"
        )
    )

    # Test for 3 league keys.
    expect_equal(
        y_league_settings_uri_gen_test_fn(c(league_id_1, league_id_2, league_id_3)),
        stringr::str_c(
            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=",
            stringr::str_flatten(c(league_id_1, league_id_2, league_id_3), collapse = ","),
            "/settings?format=json"
        )
    )

    # Test duplicate game keys
    expect_equal(
        y_league_settings_uri_gen_test_fn(c(league_id_1, league_id_1)),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/settings?format=json"
    )

    # Test 1 unique and 1 duplicate league keys
    expect_equal(
        y_league_settings_uri_gen_test_fn(c(
            league_id_1, league_id_1, league_id_2
        )),
        stringr::str_c(
            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=",
            stringr::str_flatten(c(league_id_1, league_id_2), collapse = ","),
            "/settings?format=json"
        )
    )

    # Test 1 unique and 2 duplicate league keys
    expect_equal(
        y_league_settings_uri_gen_test_fn(c(
            league_id_1, league_id_1, league_id_2, league_id_2, league_id_3
        )),
        stringr::str_c(
            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=",
            stringr::str_flatten(c(league_id_1, league_id_2, league_id_3), collapse = ","),
            "/settings?format=json"
        )
    )

    # Test bad league_key
    expect_error(y_league_settings_uri_gen_test_fn(stringr::str_remove(
        league_id_1, start = 1L, end = 6L
    )))

    # Test .league_key_check
    expect_false(.league_key_check(stringr::str_sub(
        league_id_1, start = 1L, end = 5L
    )))

    # Test .league_key_check
    expect_true(.league_key_check(
        stringr::str_sub(
            league_id_1,
            start = stringr::str_locate(league_id_1, "\\.")[1],
            end = -1L
        )
    ))

})

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              HEAD WITH WAIVERS                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Response for a head with waivers league is parsed to a tibble",
                    {
                        mock_uri <-
                            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239/settings?format=json"

                        resource <-
                            "leagues"

                        r <-
                            with_mock_api({
                                purrr::map(mock_uri, .y_get_response)
                            })

                        r_parsed <-
                            purrr::map(r, .y_parse_response, "fantasy_content", resource)


                        # Test that response uri and uri are equal.
                        expect_equal(mock_uri,
                                     purrr::map_chr(r, purrr::pluck, "url"))

                        expect_true(!is.null(r_parsed))

                        expect_true(!purrr::is_empty(r_parsed))

                        preprocess <-
                            r_parsed %>%
                            purrr::flatten() %>%
                            list_pre_process_fn()

                        df <-
                            purrr::map_df(preprocess,
                                          .league_resource_parse_fn,
                                          pluck_args = list("league", 2, 1),
                                          fn = function(x) .league_settings_parse_fn(x))


                        expect_true(tibble::is_tibble(df), TRUE)


                        # Expected colnames.
                        expected_colnames <-
                            c("league_key", "league_id", "league_name", "league_url", "league_logo_url",
                              "league_draft_status", "league_num_teams", "league_edit_key",
                              "league_weekly_deadline", "league_update_timestamp", "league_scoring_type",
                              "league_type", "league_renew", "league_renewed", "league_iris_group_chat_id",
                              "league_allow_add_to_dl_extra_pos", "league_is_pro_league", "league_is_cash_league",
                              "league_current_week", "league_start_week", "league_start_date",
                              "league_end_week", "league_end_date", "league_game_code", "league_season",
                              "draft_type", "is_auction_draft", "scoring_type", "uses_playoff",
                              "has_playoff_consolation_games", "playoff_start_week", "uses_playoff_reseeding",
                              "uses_lock_eliminated_teams", "num_playoff_teams", "num_playoff_consolation_teams",
                              "has_multiweek_championship", "waiver_type", "waiver_rule", "waiver_days",
                              "uses_faab", "draft_time", "draft_pick_time", "post_draft_players",
                              "max_teams", "waiver_time", "trade_end_date", "trade_ratify_type",
                              "trade_reject_time", "player_pool", "cant_cut_list", "draft_together",
                              "sendbird_channel_url", "roster_positions", "stat_categories",
                              "max_weekly_adds", "min_games_played", "week_has_enough_qualifying_days"
                            )

                        # Test df colnames
                        expect_named(df,
                                     expected_colnames,
                                     ignore.order = TRUE,
                                     ignore.case = TRUE)

                    })

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                   POINTS                                 ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Response for a points league is parsed to a tibble",
                    {
                        mock_uri <-
                            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;321.l.35439/settings?format=json"

                        resource <-
                            "leagues"

                        r <-
                            with_mock_api({
                                purrr::map(mock_uri, .y_get_response)
                            })

                        r_parsed <-
                            purrr::map(r, .y_parse_response, "fantasy_content", resource)


                        # Test that response uri and uri are equal.
                        expect_equal(mock_uri,
                                     purrr::map_chr(r, purrr::pluck, "url"))

                        expect_true(!is.null(r_parsed))

                        expect_true(!purrr::is_empty(r_parsed))

                        preprocess <-
                            r_parsed %>%
                            purrr::flatten() %>%
                            list_pre_process_fn()

                        df <-
                            purrr::map_df(preprocess,
                                          .league_resource_parse_fn,
                                          pluck_args = list("league", 2, 1),
                                          fn = function(x) .league_settings_parse_fn(x))


                        expect_true(tibble::is_tibble(df), TRUE)


                        # Expected colnames.
                        expected_colnames <-
                            c("league_key", "league_id", "league_name", "league_url", "league_logo_url",
                              "league_draft_status", "league_num_teams", "league_edit_key",
                              "league_weekly_deadline", "league_update_timestamp", "league_scoring_type",
                              "league_type", "league_renew", "league_renewed", "league_iris_group_chat_id",
                              "league_allow_add_to_dl_extra_pos", "league_is_pro_league", "league_is_cash_league",
                              "league_current_week", "league_start_week", "league_start_date",
                              "league_end_week", "league_end_date", "league_is_finished", "league_game_code",
                              "league_season", "draft_type", "is_auction_draft", "scoring_type",
                              "uses_playoff", "has_playoff_consolation_games", "playoff_start_week",
                              "uses_playoff_reseeding", "uses_lock_eliminated_teams", "num_playoff_teams",
                              "num_playoff_consolation_teams", "has_multiweek_championship",
                              "waiver_type", "waiver_rule", "uses_faab", "draft_time", "draft_pick_time",
                              "post_draft_players", "max_teams", "waiver_time", "trade_end_date",
                              "trade_ratify_type", "trade_reject_time", "player_pool", "cant_cut_list",
                              "draft_together", "can_trade_draft_picks", "sendbird_channel_url",
                              "roster_positions", "stat_categories", "stat_modifiers", "min_games_played",
                              "week_has_enough_qualifying_days")

                        # Test df colnames
                        expect_named(df,
                                     expected_colnames,
                                     ignore.order = TRUE,
                                     ignore.case = TRUE)

                    })


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                 HEAD POINT                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Response for a head points league is parsed to a tibble",
                    {
                        mock_uri <-
                            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;352.l.60533/settings?format=json"

                        resource <-
                            "leagues"

                        r <-
                            with_mock_api({
                                purrr::map(mock_uri, .y_get_response)
                            })

                        r_parsed <-
                            purrr::map(r, .y_parse_response, "fantasy_content", resource)


                        # Test that response uri and uri are equal.
                        expect_equal(mock_uri,
                                     purrr::map_chr(r, purrr::pluck, "url"))

                        expect_true(!is.null(r_parsed))

                        expect_true(!purrr::is_empty(r_parsed))

                        preprocess <-
                            r_parsed %>%
                            purrr::flatten() %>%
                            list_pre_process_fn()

                        df <-
                            purrr::map_df(preprocess,
                                          .league_resource_parse_fn,
                                          pluck_args = list("league", 2, 1),
                                          fn = function(x) .league_settings_parse_fn(x))


                        expect_true(tibble::is_tibble(df), TRUE)


                        # Expected colnames.
                        expected_colnames <-
                            c("league_key", "league_id", "league_name", "league_url", "league_logo_url",
                              "league_draft_status", "league_num_teams", "league_edit_key",
                              "league_weekly_deadline", "league_update_timestamp", "league_scoring_type",
                              "league_type", "league_renew", "league_renewed", "league_iris_group_chat_id",
                              "league_allow_add_to_dl_extra_pos", "league_is_pro_league", "league_is_cash_league",
                              "league_current_week", "league_start_week", "league_start_date",
                              "league_end_week", "league_end_date", "league_is_finished", "league_game_code",
                              "league_season", "draft_type", "is_auction_draft", "scoring_type",
                              "uses_playoff", "has_playoff_consolation_games", "playoff_start_week",
                              "uses_playoff_reseeding", "uses_lock_eliminated_teams", "num_playoff_teams",
                              "num_playoff_consolation_teams", "has_multiweek_championship",
                              "waiver_type", "waiver_rule", "uses_faab", "draft_time", "draft_pick_time",
                              "post_draft_players", "max_teams", "waiver_time", "trade_end_date",
                              "trade_ratify_type", "trade_reject_time", "player_pool", "cant_cut_list",
                              "draft_together", "can_trade_draft_picks", "sendbird_channel_url",
                              "roster_positions", "stat_categories", "stat_modifiers", "min_games_played",
                              "week_has_enough_qualifying_days")

                        # Test df colnames
                        expect_named(df,
                                     expected_colnames,
                                     ignore.order = TRUE,
                                     ignore.case = TRUE)

                    })


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                    ROTO                                  ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

testthat::test_that("Response for a roto league is parsed to a tibble",
                    {
                        mock_uri <-
                            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1213/settings?format=json"

                        resource <-
                            "leagues"

                        r <-
                            with_mock_api({
                                purrr::map(mock_uri, .y_get_response)
                            })

                        r_parsed <-
                            purrr::map(r, .y_parse_response, "fantasy_content", resource)


                        # Test that response uri and uri are equal.
                        expect_equal(mock_uri,
                                     purrr::map_chr(r, purrr::pluck, "url"))

                        expect_true(!is.null(r_parsed))

                        expect_true(!purrr::is_empty(r_parsed))

                        preprocess <-
                            r_parsed %>%
                            purrr::flatten() %>%
                            list_pre_process_fn()

                        df <-
                            purrr::map_df(preprocess,
                                          .league_resource_parse_fn,
                                          pluck_args = list("league", 2, 1),
                                          fn = function(x) .league_settings_parse_fn(x))


                        expect_true(tibble::is_tibble(df), TRUE)


                        # Expected colnames.
                        expected_colnames <-
                            c("league_key", "league_id", "league_name", "league_url", "league_logo_url",
                              "league_draft_status", "league_num_teams", "league_edit_key",
                              "league_weekly_deadline", "league_update_timestamp", "league_scoring_type",
                              "league_type", "league_renew", "league_renewed", "league_iris_group_chat_id",
                              "league_allow_add_to_dl_extra_pos", "league_is_pro_league", "league_is_cash_league",
                              "league_start_date", "league_end_date", "league_game_code", "league_season",
                              "draft_type", "is_auction_draft", "scoring_type", "uses_playoff",
                              "waiver_type", "waiver_rule", "waiver_days", "uses_faab", "draft_time",
                              "draft_pick_time", "post_draft_players", "max_teams", "waiver_time",
                              "trade_end_date", "trade_ratify_type", "trade_reject_time", "player_pool",
                              "cant_cut_list", "draft_together", "is_publicly_viewable", "sendbird_channel_url",
                              "roster_positions", "stat_categories", "max_adds")

                        # Test df colnames
                        expect_named(df,
                                     expected_colnames,
                                     ignore.order = TRUE,
                                     ignore.case = TRUE)

                    })


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  HEAD ONE                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



testthat::test_that("Response for a head one league is parsed to a tibble",
                    {
                        mock_uri <-
                            "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1245/settings?format=json"

                        resource <-
                            "leagues"

                        r <-
                            with_mock_api({
                                purrr::map(mock_uri, .y_get_response)
                            })

                        r_parsed <-
                            purrr::map(r, .y_parse_response, "fantasy_content", resource)


                        # Test that response uri and uri are equal.
                        expect_equal(mock_uri,
                                     purrr::map_chr(r, purrr::pluck, "url"))

                        expect_true(!is.null(r_parsed))

                        expect_true(!purrr::is_empty(r_parsed))

                        preprocess <-
                            r_parsed %>%
                            purrr::flatten() %>%
                            list_pre_process_fn()

                        df <-
                            purrr::map_df(preprocess,
                                          .league_resource_parse_fn,
                                          pluck_args = list("league", 2, 1),
                                          fn = function(x) .league_settings_parse_fn(x))


                        expect_true(tibble::is_tibble(df), TRUE)


                        # Expected colnames.
                        expected_colnames <-
                            c("league_key", "league_id", "league_name", "league_url", "league_logo_url",
                              "league_password", "league_draft_status", "league_num_teams",
                              "league_edit_key", "league_weekly_deadline", "league_update_timestamp",
                              "league_scoring_type", "league_type", "league_renew", "league_renewed",
                              "league_iris_group_chat_id", "league_short_invitation_url", "league_allow_add_to_dl_extra_pos",
                              "league_is_pro_league", "league_is_cash_league", "league_current_week",
                              "league_start_week", "league_start_date", "league_end_week",
                              "league_end_date", "league_game_code", "league_season", "draft_type",
                              "is_auction_draft", "scoring_type", "uses_playoff", "has_playoff_consolation_games",
                              "playoff_start_week", "uses_playoff_reseeding", "uses_lock_eliminated_teams",
                              "num_playoff_teams", "num_playoff_consolation_teams", "has_multiweek_championship",
                              "uses_roster_import", "roster_import_deadline", "waiver_type",
                              "waiver_rule", "uses_faab", "draft_time", "draft_pick_time",
                              "post_draft_players", "max_teams", "waiver_time", "trade_end_date",
                              "trade_ratify_type", "trade_reject_time", "player_pool", "cant_cut_list",
                              "draft_together", "sendbird_channel_url", "roster_positions",
                              "stat_categories", "divisions", "max_weekly_adds", "min_games_played",
                              "week_has_enough_qualifying_days")

                        # Test df colnames
                        expect_named(df,
                                     expected_colnames,
                                     ignore.order = TRUE,
                                     ignore.case = TRUE)

                    })


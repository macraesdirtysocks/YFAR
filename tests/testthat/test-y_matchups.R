library(YFAR)

testthat::test_that("uri is generated properly", {
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                                                            ~~
    ##                                URI FUNCTION                              ----
    ##                                                                            ~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    y_matchups_uri_gen_test_fn <- function(team_key, week = NULL) {
        resource <- "teams"
        subresource <- "matchups"
        uri_out <- "team_keys="

        # Check if keys are type league, remove FALSE and duplicates.
        key <- .single_resource_key_check(team_key, .team_key_check)


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

        # Create week param ";week=1,2,3".  NULL will eliminate ";week" and all completed weeks will be returned.
        key_path <- .uri_path_packer(key)

        # Create paths using .uri_path_package(key) to pack player keys into groups of 25.
        uri_parsed$params <-
            stringr::str_c(uri_out, key_path, "/", subresource, sep = "")

        # Check game date validity.
        if(!is.null(week)){
            week_checked <- week[.week_format_check(week)] %>% vctrs::vec_unique()
        } else{
            week_checked <- NULL
        }

        if(!is.null(week_checked) & !vctrs::vec_is_empty(week_checked)){
            week_param <- stringr::str_c("type=week;weeks=",stringr::str_flatten(week_checked, collapse = ","))
            uri_parsed$params <- stringr::str_glue(uri_parsed$params, week_param, .sep = ";")
        }

        # Build uris.
        uri <- httr::build_url(uri_parsed)
    }


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                                                            ~~
    ##                                  TEST URI                                ----
    ##                                                                            ~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    expect_identical(
        y_matchups_uri_gen_test_fn("411.l.1239.t.1"),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1/matchups?format=json"
    )

    expect_identical(
        y_matchups_uri_gen_test_fn("411.l.1239.t.1", week = 1),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1/matchups;type=week;weeks=1?format=json"
    )

    expect_identical(
        y_matchups_uri_gen_test_fn("411.l.1239.t.1", week = 1),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1/matchups;type=week;weeks=1?format=json"
    )

    expect_identical(
        y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8"), week = 1),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups;type=week;weeks=1?format=json"
    )

    expect_identical(
        y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8"), week = c(1,2)),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups;type=week;weeks=1,2?format=json"
    )

    expect_identical(
        y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8", "411"), week = c(1,2, "2022-01-01")),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups;type=week;weeks=1,2?format=json"
    )

    expect_identical(
        y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8", "411", "411.p.1000"), week = c("2022-01-01")),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups?format=json"
    )

    expect_identical(
        y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8", "411", "411.p.1000"), week = c(1,2, "2022-01-01")),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups;type=week;weeks=1,2?format=json"
    )

    expect_error(
        y_matchups_uri_gen_test_fn(c("411", "411.p.1000"), week = c(1,2))

    )


})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            Y_MATCHUPS ONE TEAM                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Response for a single team is parsed to a tibble",
                    {
                        # Declare desired uri.
                        mock_uri <-
                            "https://fantasysports.yahooapis.com/fantasy/v2/teams;411.l.1239.t.1/matchups?format=json"

                        # Expected resource
                        resource <-
                            "teams"

                        # Get response.
                        r <-
                            with_mock_api({
                                purrr::map(mock_uri, .y_get_response)
                            })

                        # Parsed response.
                        r_parsed <-
                            purrr::map(r, .y_parse_response, "fantasy_content", resource)

                        # Test that response uri and declared uri are equal.
                        expect_equal(mock_uri,
                                     purrr::map_chr(r, purrr::pluck, "url"))

                        # Test not null.
                        expect_true(!is.null(r_parsed))

                        # Test not empty.
                        expect_true(!purrr::is_empty(r_parsed))

                        # Preprocess list.  This step is verbatim from y_matchups.
                        preprocess <-
                            # General preprocess steps that most functions use.
                            r_parsed %>%
                            purrr::flatten() %>%
                            purrr::keep(purrr::is_list) %>%
                            # Pluck out matchup element.
                            purrr::map(purrr::pluck, "team", 2, "matchups") %>%
                            # Discard weeks that havent
                            purrr::map_depth(2, function(x) {
                                purrr::discard(x, purrr::has_element, "preevent")
                            }) %>%
                            # Remove empty lists created by discard.
                            purrr::map_depth(1, function(x) {
                                purrr::compact(x) %>% purrr::keep(purrr::is_list)
                            })

                        # Parse response to tibble.
                        df <-
                            preprocess %>%
                            purrr::map_depth(2, .matchup_parse_fn) %>%
                            purrr::map_df(dplyr::bind_rows)

                        # Test that a tibble was returned from parsing.
                        expect_true(tibble::is_tibble(df), TRUE)


                        # Expected colnames.
                        expected_colnames <-
                            c(
                                "week",
                                "week_start",
                                "week_end",
                                "status",
                                "is_playoffs",
                                "is_consolation",
                                "is_tied",
                                "winner_team_key",
                                "team_key",
                                "team_id",
                                "team_name",
                                "team_url",
                                "team_logo_size",
                                "team_logo_url",
                                "team_waiver_priority",
                                "team_faab_balance",
                                "team_number_of_moves",
                                "team_number_of_trades",
                                "team_coverage_type",
                                "team_coverage_value",
                                "team_value",
                                "team_league_scoring_type",
                                "team_draft_position",
                                "team_has_draft_grade",
                                "team_manager_manager_id",
                                "team_manager_nickname",
                                "team_manager_guid",
                                "team_manager_felo_score",
                                "team_manager_felo_tier",
                                "count_g",
                                "count_a",
                                "count_x",
                                "count_ppp",
                                "count_sog",
                                "count_hit",
                                "count_w",
                                "count_ga",
                                "count_gaa",
                                "count_sv",
                                "count_sa",
                                "count_sv_percent",
                                "count_sho",
                                "coverage_type",
                                "week_2",
                                "total",
                                "total_remaining_games",
                                "total_live_games",
                                "total_completed_games",
                                "g_winner",
                                "a_winner",
                                "x_winner",
                                "ppp_winner",
                                "sog_winner",
                                "hit_winner",
                                "w_winner",
                                "gaa_winner",
                                "sv_percent_winner",
                                "sho_winner"
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
##                            Y_MATCHUPS TWO TEAMS                          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Response for two teams is parsed to a tibble",
                    {
                        # Declare desired uri.
                        mock_uri <-
                            "https://fantasysports.yahooapis.com/fantasy/v2/teams;411.l.1239.t.1,411.l.1240.t.8/matchups?format=json"

                        # Expected resource
                        resource <-
                            "teams"

                        # Get response.
                        r <-
                            with_mock_api({
                                purrr::map(mock_uri, .y_get_response)
                            })

                        # Parsed response.
                        r_parsed <-
                            purrr::map(r, .y_parse_response, "fantasy_content", resource)

                        # Test that response uri and declared uri are equal.
                        expect_equal(mock_uri,
                                     purrr::map_chr(r, purrr::pluck, "url"))

                        # Test not null.
                        expect_true(!is.null(r_parsed))

                        # Test not empty.
                        expect_true(!purrr::is_empty(r_parsed))

                        # Preprocess list.  This step is verbatim from y_matchups.
                        preprocess <-
                            # General preprocess steps that most functions use.
                            r_parsed %>%
                            purrr::flatten() %>%
                            purrr::keep(purrr::is_list) %>%
                            # Pluck out matchup element.
                            purrr::map(purrr::pluck, "team", 2, "matchups") %>%
                            # Discard weeks that havent
                            purrr::map_depth(2, function(x) {
                                purrr::discard(x, purrr::has_element, "preevent")
                            }) %>%
                            # Remove empty lists created by discard.
                            purrr::map_depth(1, function(x) {
                                purrr::compact(x) %>% purrr::keep(purrr::is_list)
                            })


                        df <-
                            preprocess %>%
                            purrr::map_depth(2, .matchup_parse_fn) %>%
                            purrr::map_df(dplyr::bind_rows)

                        # Parse response to df.
                        df <-
                            preprocess %>%
                            purrr::map_depth(2, .matchup_parse_fn) %>%
                            purrr::map_df(dplyr::bind_rows)

                        # Test that a tibble was returned from parsing.
                        expect_true(tibble::is_tibble(df), TRUE)


                        # Expected colnames.
                        expected_colnames <-
                            c(
                                "week",
                                "week_start",
                                "week_end",
                                "status",
                                "is_playoffs",
                                "is_consolation",
                                "is_tied",
                                "winner_team_key",
                                "team_key",
                                "team_id",
                                "team_name",
                                "team_url",
                                "team_logo_size",
                                "team_logo_url",
                                "team_waiver_priority",
                                "team_faab_balance",
                                "team_number_of_moves",
                                "team_number_of_trades",
                                "team_coverage_type",
                                "team_coverage_value",
                                "team_value",
                                "team_league_scoring_type",
                                "team_draft_position",
                                "team_has_draft_grade",
                                "team_manager_manager_id",
                                "team_manager_nickname",
                                "team_manager_guid",
                                "team_manager_felo_score",
                                "team_manager_felo_tier",
                                "count_g",
                                "count_a",
                                "count_x",
                                "count_ppp",
                                "count_sog",
                                "count_hit",
                                "count_w",
                                "count_ga",
                                "count_gaa",
                                "count_sv",
                                "count_sa",
                                "count_sv_percent",
                                "count_sho",
                                "coverage_type",
                                "week_2",
                                "total",
                                "total_remaining_games",
                                "total_live_games",
                                "total_completed_games",
                                "g_winner",
                                "a_winner",
                                "x_winner",
                                "ppp_winner",
                                "sog_winner",
                                "hit_winner",
                                "w_winner",
                                "gaa_winner",
                                "sv_percent_winner",
                                "sho_winner",
                                "team_division_id",
                                "count_p",
                                "count_pim",
                                "count_blk",
                                "p_winner",
                                "pim_winner",
                                "blk_winner",
                                "team_manager_manager_id_2",
                                "team_manager_nickname_2",
                                "team_manager_guid_2",
                                "team_manager_is_comanager",
                                "team_manager_felo_score_2",
                                "team_manager_felo_tier_2",
                                "team_manager_is_commissioner",
                                "team_manager_is_commissioner_2"
                            )

                        # Test df colnames
                        expect_named(df,
                                     expected_colnames,
                                     ignore.order = TRUE,
                                     ignore.case = TRUE)

                    })

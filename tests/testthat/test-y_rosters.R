library(YFAR)

test_that("y_rosters uri generates properly",{

    y_rosters_uri_gen_test_fn <- function(key = NULL, game_date = NULL){

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Eligible key types.
        e_key_types <- c("leagues", "teams")

        # Assign a resource to each key and count.
        # Function then selects most frequently occurring resource and assigns value to resource.
        c(resource, key, .) %<-% .multiple_resource_key_check(key, e_key_types = e_key_types)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # resource assigned in if statement above.
        subresource <- switch(resource, "leagues" = "teams", "teams" = NULL)
        collection = "roster"
        uri_out <- switch(resource,"leagues" = "league_keys=", "teams" = "team_keys=")
        game_key <- .game_key_assign_fn(key)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

        # Pack key paths.
        key_path <- .uri_path_packer(key, 25)

        # Inital params.
        uri_parsed$params <- stringr::str_c(uri_out, key_path, sep = "")

        # Check game date validity.
        game_date_checked <-
            if(!is.null(game_date)){
                game_date[.date_format_check(game_date)]
            } else{
                NULL
            }

        # Update uri params conditional on resource.
        if(identical(resource, "leagues")){
            uri_parsed$params <- stringr::str_c(uri_parsed$params, subresource, collection, sep = "/")
        } else if(identical(resource, "teams")){
            uri_parsed$params <- stringr::str_c(uri_parsed$params, collection, sep = "/")
        } else{
            uri_parsed$params <- uri_parsed$params
        }

        # If game_date_checked is not null create date_params and append to params,
        if(!is.null(game_date_checked) & !vctrs::vec_is_empty(game_date_checked)){
            date_param <- stringr::str_c("type=date;date", game_date_checked, sep = "=")
            uri_parsed$params <- stringr::str_c(uri_parsed$params, date_param, sep = ";")
        }

        # Build uri.
        uri <- httr::build_url(uri_parsed)
    }

    expect_identical(
        y_rosters_uri_gen_test_fn("411.l.1239"),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/teams/roster?format=json"
    )

    expect_identical(
        y_rosters_uri_gen_test_fn(c("411.l.1239", "411.l.1240"), game_date = c("2022-01-30", "2022-01-31")),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/teams/roster;type=date;date=2022-01-30?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/teams/roster;type=date;date=2022-01-31?format=json")
    )

    expect_identical(
        y_rosters_uri_gen_test_fn(c("411.l.1245.t.8"), game_date = c("2022-01-30", "2022-01-31")),
        c("https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1245.t.8/roster;type=date;date=2022-01-30?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1245.t.8/roster;type=date;date=2022-01-31?format=json")
        )

    # expect_identical(
    #     y_rosters_uri_gen_test_fn(c("411.l.1239.t.8", "411.l.1239"), game_date = c("2022-01-30", "2022-01-31")),
    #     c(
    #         "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239.t.8/teams/roster;type=date;date=2022-01-30?format=json",
    #         "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239.t.8/teams/roster;type=date;date=2022-01-31?format=json"
    #     ))

    expect_identical(
        y_rosters_uri_gen_test_fn(c("411.l.1239.t.8", "411.l.1239", "411.l.1240.t.8", "222.p.1239"), game_date = c("2022-01-30", "2022-01-31")),
        c("https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-30?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-31?format=json")
    )

    expect_identical(
        y_rosters_uri_gen_test_fn(c("411.l.1239.t.8", "411.l.1239", "411.l.1240.t.8", "222.p.1239"), game_date = c("2022-01-29", "2022-01-30", "2022-01-31")),
        c("https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-29?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-30?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-31?format=json")
    )

    expect_identical(
        y_rosters_uri_gen_test_fn(c("411.l.1239.t.8", "411.l.1239", "411.l.1240.t.8", "222.p.1239"), game_date = c("2022-01-29", "2022-01-30", "2022-01-31", 1)),
        c("https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-29?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-30?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-31?format=json")
    )

    expect_identical(
        y_rosters_uri_gen_test_fn(c("411.l.1239.t.8", "411.l.1239", "411.l.1240.t.8", "222.p.1239", "411.l.1239.t.8"), game_date = c("2022-01-29", "2022-01-30", "2022-01-31")),
        c("https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-29?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-30?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1240.t.8/roster;type=date;date=2022-01-31?format=json")
    )


})



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                 LEAGUE KEY                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("League key is parsed to tibble",{

    # Declare desired uri.
    uri <- "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239/teams/roster?format=json"


    # Expected resource
    resource <-
        "leagues"

    # Get response.
    r <-
        with_mock_api({
            purrr::map(uri, .y_get_response)
        })

    # Parsed response.
    r_parsed <-
        purrr::map(r, .y_parse_response, "fantasy_content", resource)

    # Test that response uri and declared uri are equal.
    expect_equal(uri,
                 purrr::map_chr(r, purrr::pluck, "url"))

    # Test not null.
    expect_true(!is.null(r_parsed))

    # Test not empty.
    expect_true(!purrr::is_empty(r_parsed))

    # Define parse function relative to resource value.


    # Preprocess list.  This step is verbatim from y_matchups.
    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        purrr::keep(purrr::is_list)

    df <-
        purrr::map_df(
            preprocess,
            .league_resource_parse_fn,
            .team_resource_parse_fn,
            .roster_resource_parse_fn
        )

    # Test that a tibble was returned from parsing.
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
          "team_key", "team_id", "team_name", "team_url", "team_logo_size",
          "team_logo_url", "team_waiver_priority", "team_faab_balance",
          "team_number_of_moves", "team_number_of_trades", "team_coverage_type",
          "team_coverage_value", "team_value", "team_league_scoring_type",
          "team_draft_position", "team_has_draft_grade", "team_manager_manager_id",
          "team_manager_nickname", "team_manager_guid", "team_manager_felo_score",
          "team_manager_felo_tier", "roster_coverage_type", "roster_date",
          "roster_is_editable", "player_key", "player_id", "player_full",
          "player_first", "player_last", "player_ascii_first", "player_ascii_last",
          "player_editorial_player_key", "player_editorial_team_key", "player_editorial_team_full_name",
          "player_editorial_team_abbr", "player_uniform_number", "player_display_position",
          "player_url", "player_size", "player_image_url", "player_is_undroppable",
          "player_position_type", "player_primary_position", "player_position",
          "player_has_player_notes", "player_notes_last_timestamp", "selected_position_coverage_type",
          "selected_position_date", "selected_position_position", "selected_position_is_flex",
          "player_has_recent_player_notes", "player_status", "player_status_full",
          "player_injury_note", "player_on_disabled_list", "coverage_type_coverage_type",
          "coverage_value_coverage_value", "value_value", "starting_status_coverage_type",
          "starting_status_date", "starting_status_is_starting")


    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)

})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  TEAM KEY                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Teamkey is parsed to tibble",{

    # Declare desired uri.
    uri <- "https://fantasysports.yahooapis.com/fantasy/v2/teams;411.l.1239.t.1/roster?format=json"


    # Expected resource
    resource <-
        "teams"

    # Get response.
    r <-
        with_mock_api({
            purrr::map(uri, .y_get_response)
        })

    # Parsed response.
    r_parsed <-
        purrr::map(r, .y_parse_response, "fantasy_content", resource)

    # Test that response uri and declared uri are equal.
    expect_equal(uri,
                 purrr::map_chr(r, purrr::pluck, "url"))

    # Test not null.
    expect_true(!is.null(r_parsed))

    # Test not empty.
    expect_true(!purrr::is_empty(r_parsed))

    # Define parse function relative to resource value.


    # Preprocess list.  This step is verbatim from y_matchups.
    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        purrr::keep(purrr::is_list)

    df <-
        purrr::map_df(
            preprocess,
            .team_resource_parse_fn,
            .roster_resource_parse_fn
        )

    # Test that a tibble was returned from parsing.
    expect_true(tibble::is_tibble(df), TRUE)


    # Expected colnames.
    expected_colnames <-
        c("team_key", "team_id", "team_name", "team_url", "team_logo_size",
          "team_logo_url", "team_waiver_priority", "team_faab_balance",
          "team_number_of_moves", "team_number_of_trades", "team_coverage_type",
          "team_coverage_value", "team_value", "team_league_scoring_type",
          "team_draft_position", "team_has_draft_grade", "team_manager_manager_id",
          "team_manager_nickname", "team_manager_guid", "team_manager_felo_score",
          "team_manager_felo_tier", "roster_coverage_type", "roster_date",
          "roster_is_editable", "player_key", "player_id", "player_full",
          "player_first", "player_last", "player_ascii_first", "player_ascii_last",
          "player_editorial_player_key", "player_editorial_team_key", "player_editorial_team_full_name",
          "player_editorial_team_abbr", "player_uniform_number", "player_display_position",
          "player_url", "player_size", "player_image_url", "player_is_undroppable",
          "player_position_type", "player_primary_position", "player_position",
          "player_has_player_notes", "player_notes_last_timestamp", "selected_position_coverage_type",
          "selected_position_date", "selected_position_position", "selected_position_is_flex",
          "player_has_recent_player_notes", "player_status", "player_status_full",
          "player_injury_note", "player_on_disabled_list", "coverage_type_coverage_type",
          "coverage_value_coverage_value", "value_value")


    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)

})

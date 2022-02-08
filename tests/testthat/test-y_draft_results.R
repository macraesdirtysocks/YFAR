library(YFAR)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  TEST URI                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("y_draft_results uri generates properly",{

    y_draft_results_uri_gen_test_fn <- function(key){

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
    collection <- "draftresults"
    uri_out <- switch(resource, "leagues" = "league_keys=", "teams" = "team_keys=")

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

    # Pack paths into lengths of 25.
    key_path <-
        .uri_path_packer(key, 25)

    # Initial uri params.
    uri_parsed$params <-
        stringr::str_c(uri_out, key_path, sep = "")

    # Other params condition on resource.
    if(!is.null(subresource)){
        uri_parsed$params <- stringr::str_c(uri_parsed$params, subresource, collection, sep = "/")
    } else{
        uri_parsed$params <- stringr::str_c(uri_parsed$params, collection, sep = "/")
    }

    # Build uris.
    uri <- httr::build_url(uri_parsed)

    }

    expect_identical(
        y_draft_results_uri_gen_test_fn("411.l.1239"),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/teams/draftresults?format=json"
    )

    expect_identical(
        y_draft_results_uri_gen_test_fn(c("411.l.1239", "411.l.1240")),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/teams/draftresults?format=json"
    )

    expect_identical(
        y_draft_results_uri_gen_test_fn(c("411.l.1239.t.1")),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1/draftresults?format=json"
    )

    expect_identical(
        y_draft_results_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1240.t.8")),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1240.t.8/draftresults?format=json"
    )

    expect_identical(
        y_draft_results_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1240.t.8", "411.l.1239")),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1240.t.8/draftresults?format=json"
    )

    expect_identical(
        y_draft_results_uri_gen_test_fn(c("411.l.1239", "411.l.1240", "411.l.1240.t.1")),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/teams/draftresults?format=json"
    )

})

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                LEAGUES URI                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



testthat::test_that("Leagues uri returns valid response and is parsed to a tibble",{

    # Declare desired mock uri.
    mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239/teams/draftresults?format=json"

    # Expected resource
    resource <-
        "leagues"

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

    # Preprocess parsed content.
    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        purrr::keep(purrr::is_list)

    # DF
    df <-
        purrr::map_df(
            preprocess,
            .league_resource_parse_fn,
            .team_resource_parse_fn,
            .two_deep_subresource_parse
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
          "team_manager_felo_tier", "draft_result_pick", "draft_result_round",
          "draft_result_team_key", "draft_result_player_key")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)
})



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  TEAMS URI                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




testthat::test_that("Teams uri returns valid response and is parsed to a tibble",{

    # Declare desired mock uri.
    mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;411.l.1239.t.1,411.l.1240.t.8/draftresults?format=json"

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

    # Preprocess parsed content.
    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        purrr::keep(purrr::is_list)

    # DF
    df <-
        purrr::map_df(preprocess,
                       .team_resource_parse_fn,
                       .two_deep_subresource_parse)

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
          "team_manager_felo_tier", "draft_result_pick", "draft_result_round",
          "draft_result_team_key", "draft_result_player_key", "team_division_id"
        )

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)
})

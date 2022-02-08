library(YFAR)

test_that("y_teams uri generates properly",{

    y_teams_uri_gen_test_fn <- function(league_key = NULL){

      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                                  ARGUMENTS                               ----
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      resource <- "leagues"
      subresource <- "teams"
      uri_out <- "league_keys="

      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                                    CHECKS                                ----
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Check if keys are type league, remove FALSE and duplicates.
      key <- .single_resource_key_check(league_key, .league_key_check)

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

        key_path <-
            .uri_path_packer(key, 25)

        uri_parsed$params <-
            stringr::str_c(uri_out, key_path, "/", subresource)

        uri <- httr::build_url(uri_parsed)

    }


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                                                            ~~
    ##                                  TEST URI                                ----
    ##                                                                            ~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Single league key.
    expect_identical(
        y_teams_uri_gen_test_fn("411.l.1239"),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/teams?format=json"
        )

    # 2 league keys
    expect_identical(
        y_teams_uri_gen_test_fn(c("411.l.1239", "411.l.1240")),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/teams?format=json"
        )

    # 1 good league key and 1 bad
    expect_identical(
        y_teams_uri_gen_test_fn(c("411.l.1239", "411.p.1240")),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/teams?format=json"
        )

    # 1 bad league key.
    expect_error(
        y_teams_uri_gen_test_fn(c("411.p.1240"))
        )

    # 3 good league key and 1 bad
    expect_identical(
      y_teams_uri_gen_test_fn(c("411.l.1239", "411.p.1240", "411.l.1240", "406.l.1111")),
      "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240,406.l.1111/teams?format=json"
    )


})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                 SINGLE KEY                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



testthat::test_that("Single league resource is parsed to a tibble",{

    # Declare desired uri.
    mock_uri <- "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239/teams?format=json"

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

    # Preprocess list.
    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        purrr::keep(purrr::is_list)

    df <-
        preprocess %>%
        purrr::map_df(.league_resource_parse_fn, .team_meta_parse_fn)

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
          "team_manager_felo_tier")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)

})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  TWO KEYS                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("2 league resources is parsed to a tibble",{
# Declare desired uri.
  mock_uri <- "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239,411.l.1240/teams?format=json"


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

# Preprocess list.
preprocess <-
    r_parsed %>%
    purrr::flatten() %>%
    purrr::keep(purrr::is_list)

df <-
    preprocess %>%
    purrr::map_df(.league_resource_parse_fn, .team_meta_parse_fn)

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
      "team_manager_felo_tier", "team_division_id", "team_manager_is_commissioner",
      "team_manager_is_comanager")

# Test df colnames
expect_named(df,
             expected_colnames,
             ignore.order = TRUE,
             ignore.case = TRUE)

})

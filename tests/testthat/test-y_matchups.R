library(YFAR)

testthat::test_that("uri is generated properly", {

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                                URI FUNCTION                              ----
  ##                                                                            ~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  y_matchups_uri_gen_test_fn <- function(team_key, week = NULL) {
    resource <- "teams"
    subresource <- "matchups"
    uri_out <- "team_keys="

    # Check if keys are type league, remove FALSE and duplicates.
    key <- .single_resource_key_check(team_key, .team_key_check)


    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                     URI                                  ----
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    if (!is.null(week)) {
      week_checked <- week[.week_format_check(week)] %>% vctrs::vec_unique()
    } else {
      week_checked <- NULL
    }

    if (!is.null(week_checked) & !vctrs::vec_is_empty(week_checked)) {
      week_param <- stringr::str_c("type=week;weeks=", stringr::str_flatten(week_checked, collapse = ","))
      uri_parsed$params <- stringr::str_glue(uri_parsed$params, week_param, .sep = ";")
    }

    # Build uris.
    uri <- httr::build_url(uri_parsed)
  }


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                                  TEST URI                                ----
  ##                                                                            ~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8"), week = c(1, 2)),
    "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups;type=week;weeks=1,2?format=json"
  )

  expect_identical(
    y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8", "411"), week = c(1, 2, "2022-01-01")),
    "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups;type=week;weeks=1,2?format=json"
  )

  expect_identical(
    y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8", "411", "411.p.1000"), week = c("2022-01-01")),
    "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups?format=json"
  )

  expect_identical(
    y_matchups_uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8", "411", "411.p.1000"), week = c(1, 2, "2022-01-01")),
    "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8/matchups;type=week;weeks=1,2?format=json"
  )

  expect_error(
    y_matchups_uri_gen_test_fn(c("411", "411.p.1000"), week = c(1, 2))
  )
})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            Y_MATCHUPS ONE TEAM                           ----
##                                                                            ~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Response for a single team is parsed to a tibble", {
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
  expect_equal(
    mock_uri,
    purrr::map_chr(r, purrr::pluck, "url")
  )

  # Test not null.
  expect_true(!is.null(r_parsed))

  # Test not empty.
  expect_true(!purrr::is_empty(r_parsed))

  # Preprocess list.  This step is verbatim from y_matchups.
  preprocess <-
    r_parsed %>%
    purrr::flatten() %>%
    purrr::keep(purrr::is_list) %>%
    list_pre_process_fn()

  # If response is form a non H2H league the mathcup elements will contain an error message.
  # Extract error elements and create a message for printing.
  error_elements <-
    preprocess %>%
    rrapply::rrapply(
      classes = "list",
      condition = function(x, .xname) .xname == "exception",
      f = function(x) purrr::pluck(x, "message"),
      how = "melt"
    ) %>%
    dplyr::select("L1", "value") %>%
    dplyr::mutate(message = stringr::str_c("Error in", L1, "with message:", value, sep = " "))

  # If errors exist print.
  if(nrow(error_elements) > 0){
    message(crayon::cyan$bold("Matchups removed:\n", paste0(error_elements$message, "\n")))
  }

  # Non error list elelments.
  good_elements <- dplyr::setdiff(names(preprocess), error_elements %>% dplyr::pull("L1"))

  # Subset good elements and pluck matchup data.
  preprocess <-
    preprocess[good_elements] %>%
    purrr::map(purrr::pluck, "team", 2, "matchups")


  df <-
        preprocess %>%
        purrr::map_depth(2, .matchup_parse_fn) %>%
        purrr::map_df(dplyr::bind_rows)

  # Test that a tibble was returned from parsing.
  expect_true(tibble::is_tibble(df), TRUE)


  # Expected colnames.
  expected_colnames <-
    c("matchup_week", "matchup_week_start", "matchup_week_end", "matchup_status",
      "matchup_is_playoffs", "matchup_is_consolation", "matchup_is_tied",
      "matchup_winner_team_key", "matchup_stat_winners", "matchup_team_key",
      "matchup_team_id", "matchup_team_name", "matchup_team_url", "matchup_team_logos_team_logo_size",
      "matchup_team_logos_team_logo_url", "matchup_team_waiver_priority",
      "matchup_team_faab_balance", "matchup_team_number_of_moves",
      "matchup_team_number_of_trades", "matchup_team_roster_adds_coverage_type",
      "matchup_team_roster_adds_coverage_value", "matchup_team_roster_adds_value",
      "matchup_team_league_scoring_type", "matchup_team_draft_position",
      "matchup_team_has_draft_grade", "matchup_team_managers_manager_manager_id",
      "matchup_team_managers_manager_nickname", "matchup_team_managers_manager_guid",
      "matchup_team_managers_manager_felo_score", "matchup_team_managers_manager_felo_tier",
      "matchup_team_stats_coverage_type", "matchup_team_stats_week",
      "matchup_team_stats", "matchup_team_points_coverage_type", "matchup_team_points_week",
      "matchup_team_points_total", "matchup_team_remaining_games_coverage_type",
      "matchup_team_remaining_games_week", "matchup_team_remaining_games",
      "matchup_team_live_games", "matchup_team_completed_games")

  # Test df colnames
  expect_named(df,
    expected_colnames,
    ignore.order = TRUE,
    ignore.case = TRUE
  )
})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            Y_MATCHUPS TWO TEAMS                          ----
##                                                                            ~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Response for two teams is parsed to a tibble", {
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
  expect_equal(
    mock_uri,
    purrr::map_chr(r, purrr::pluck, "url")
  )

  # Test not null.
  expect_true(!is.null(r_parsed))

  # Test not empty.
  expect_true(!purrr::is_empty(r_parsed))

  # Preprocess list.  This step is verbatim from y_matchups.
  # General preprocess steps that most functions use.
  preprocess <-
    r_parsed %>%
    purrr::flatten() %>%
    list_pre_process_fn()

  # If response is form a non H2H league the mathcup elements will contain an error message.
  # Extract error elements and create a message for printing.
  error_elements <-
    preprocess %>%
    rrapply::rrapply(
      classes = "list",
      condition = function(x, .xname) .xname == "exception",
      f = function(x) purrr::pluck(x, "message"),
      how = "melt"
    ) %>%
    dplyr::select("L1", "value") %>%
    dplyr::mutate(message = stringr::str_c("Error in", L1, "with message:", value, sep = " "))

  # If errors exist print.
  if(nrow(error_elements) > 0){
    message(crayon::cyan$bold("Matchups removed:\n", paste0(error_elements$message, "\n")))
  }

  # Non error list elelments.
  good_elements <- dplyr::setdiff(names(preprocess), error_elements %>% dplyr::pull("L1"))

  # Subset good elements and pluck matchup data.
  preprocess <-
    preprocess[good_elements] %>%
    purrr::map(purrr::pluck, "team", 2, "matchups")


  # Parse response to df.
  df <-
    preprocess %>%
    purrr::map_depth(2, .matchup_parse_fn) %>%
    purrr::map_df(dplyr::bind_rows)

  # Test that a tibble was returned from parsing.
  expect_true(tibble::is_tibble(df), TRUE)


  # Expected colnames.
  expected_colnames <-
    c("matchup_week", "matchup_week_start", "matchup_week_end", "matchup_status",
      "matchup_is_playoffs", "matchup_is_consolation", "matchup_is_tied",
      "matchup_winner_team_key", "matchup_stat_winners", "matchup_team_key",
      "matchup_team_id", "matchup_team_name", "matchup_team_url", "matchup_team_logos_team_logo_size",
      "matchup_team_logos_team_logo_url", "matchup_team_waiver_priority",
      "matchup_team_faab_balance", "matchup_team_number_of_moves",
      "matchup_team_number_of_trades", "matchup_team_roster_adds_coverage_type",
      "matchup_team_roster_adds_coverage_value", "matchup_team_roster_adds_value",
      "matchup_team_league_scoring_type", "matchup_team_draft_position",
      "matchup_team_has_draft_grade", "matchup_team_managers_manager_manager_id",
      "matchup_team_managers_manager_nickname", "matchup_team_managers_manager_guid",
      "matchup_team_managers_manager_felo_score", "matchup_team_managers_manager_felo_tier",
      "matchup_team_stats_coverage_type", "matchup_team_stats_week",
      "matchup_team_stats", "matchup_team_points_coverage_type", "matchup_team_points_week",
      "matchup_team_points_total", "matchup_team_remaining_games_coverage_type",
      "matchup_team_remaining_games_week", "matchup_team_remaining_games",
      "matchup_team_live_games", "matchup_team_completed_games", "matchup_team_division_id",
      "matchup_team_managers_manager_manager_id_2", "matchup_team_managers_manager_nickname_2",
      "matchup_team_managers_manager_guid_2", "matchup_team_managers_manager_is_comanager",
      "matchup_team_managers_manager_felo_score_2", "matchup_team_managers_manager_felo_tier_2",
      "matchup_team_managers_manager_is_commissioner", "matchup_team_managers_manager_is_commissioner_2"
    )

  # Test df colnames
  expect_named(df,
    expected_colnames,
    ignore.order = TRUE,
    ignore.case = TRUE
  )
})

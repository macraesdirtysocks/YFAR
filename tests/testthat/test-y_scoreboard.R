library(YFAR)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  TEST URI                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("y_scoreboard uri generates properly",{

    y_scoreboard_uri_gen_test_fn <- function(league_key = NULL, week = NULL){

      # Check if keys are type league, remove FALSE and duplicates.
      key <- .single_resource_key_check(league_key, .league_key_check)

        resource <- "leagues"
        subresource <- "scoreboard"
        uri_out <- "league_keys="

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

        # Pack league keys into uri's of length 25.
        key_paths <-
            .uri_path_packer(key, 25)

        uri_parsed$params <-
            stringr::str_c(uri_out, key_paths, "/", subresource, sep = "")

        if(!is.null(week)){
            week_checked <-
                suppressWarnings(week[!is.na(as.integer(as.character(week)))]) %>% vctrs::vec_unique()
        } else{
            week_checked <- NULL
        }
        # If week is not empty turn it into a param by pasting the name to the value and
        # gluing to already existing param.
        # i.e. week <- list(week=1) becomes week=1 and then type=week;week=1.
        if(!is.null(week_checked)){
            week_param <- stringr::str_c("type=week;week=", week_checked)
            uri_parsed$params <- stringr::str_c(uri_parsed$params, week_param, sep = ";")
        }

        # Build uris.
        uri <- httr::build_url(uri_parsed)
    }

    expect_identical(
        y_scoreboard_uri_gen_test_fn(league_key = "411.l.1239", week = NULL),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/scoreboard?format=json"

    )

    expect_identical(
        y_scoreboard_uri_gen_test_fn(league_key = "411.l.1239", week = 1),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/scoreboard;type=week;week=1?format=json"

    )

    expect_identical(
        y_scoreboard_uri_gen_test_fn(league_key = c("411.l.1239", "411.l.1240"), week = 1),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=1?format=json"

    )

    expect_identical(
        y_scoreboard_uri_gen_test_fn(league_key = c("411.l.1239", "411.l.1240"), week = c(1,2)),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=1?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=2?format=json")

    )

    expect_identical(
        y_scoreboard_uri_gen_test_fn(league_key = c("411.l.1239", "411.l.1240", "411"), week = c(1,2)),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=1?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=2?format=json")

    )

    expect_identical(
        y_scoreboard_uri_gen_test_fn(league_key = c("411.l.1239", "411.l.1240", "411", "411.p.6369"), week = c(1,2)),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=1?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=2?format=json")

    )

    expect_identical(
        y_scoreboard_uri_gen_test_fn(league_key = c("411.l.1239", "411.l.1240", "411", "411.p.6369"), week = c(1,2, "2022-02-01")),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=1?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=2?format=json")

    )

    expect_identical(
        y_scoreboard_uri_gen_test_fn(league_key = c("411.l.1239", "411.l.1240", "411", "411.p.6369"), week = c(1, 1, 2, "2022-02-01")),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=1?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/scoreboard;type=week;week=2?format=json")

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

    # Declare desired uri.
  mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239/scoreboard?format=json"

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

    # Subset good elements and pluck scoreboard data.
    preprocess <-
      preprocess[good_elements]

    # purrr::map_depth(2, purrr::map_at, 2, ~purrr::pluck(.x, "scoreboard" ,"0", "matchups"))
    df <-
      preprocess %>%
      purrr::map_df(
        .league_resource_parse_fn,
        pluck_args = list("league", 2, "scoreboard", "0", "matchups"),
        fn = function(x) purrr::map_df(x, .matchup_parse_fn))

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
        "matchup_week", "matchup_week_start", "matchup_week_end", "matchup_status",
        "matchup_is_playoffs", "matchup_is_consolation", "matchup_stat_winners",
        "matchup_team_key", "matchup_team_id", "matchup_team_name", "matchup_team_url",
        "matchup_team_logos_team_logo_size", "matchup_team_logos_team_logo_url",
        "matchup_team_waiver_priority", "matchup_team_faab_balance",
        "matchup_team_number_of_moves", "matchup_team_number_of_trades",
        "matchup_team_roster_adds_coverage_type", "matchup_team_roster_adds_coverage_value",
        "matchup_team_roster_adds_value", "matchup_team_league_scoring_type",
        "matchup_team_draft_position", "matchup_team_has_draft_grade",
        "matchup_team_managers_manager_manager_id", "matchup_team_managers_manager_nickname",
        "matchup_team_managers_manager_guid", "matchup_team_managers_manager_felo_score",
        "matchup_team_managers_manager_felo_tier", "matchup_team_stats_coverage_type",
        "matchup_team_stats_week", "matchup_team_stats", "matchup_team_points_coverage_type",
        "matchup_team_points_week", "matchup_team_points_total", "matchup_team_remaining_games_coverage_type",
        "matchup_team_remaining_games_week", "matchup_team_remaining_games",
        "matchup_team_live_games", "matchup_team_completed_games")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)
})



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              LEAGUES WEEK URI                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Single league with a week arg uri returns valid response and is parsed to a tibble",{

    # Declare desired mock uri.
    mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239/scoreboard;type=week;week=1?format=json"

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

    # Subset good elements and pluck scoreboard data.
    preprocess <-
      preprocess[good_elements]

    # purrr::map_depth(2, purrr::map_at, 2, ~purrr::pluck(.x, "scoreboard" ,"0", "matchups"))
    df <-
      preprocess %>%
      purrr::map_df(.league_resource_parse_fn,
                    list("league", 2, "scoreboard" , "0", "matchups"),
                    function(x)
                      purrr::map_df(x, .matchup_parse_fn))

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
        "matchup_week", "matchup_week_start", "matchup_week_end", "matchup_status",
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
                 ignore.case = TRUE)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              TWO LEAGUES URI                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Two leagues uri returns valid response and is parsed to a tibble",{

    # Declare desired mock uri.
  mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239,411.l.1240/scoreboard;type=week;week=1?format=json"

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

    # Subset good elements and pluck scoreboard data.
    preprocess <-
      preprocess[good_elements]

    # purrr::map_depth(2, purrr::map_at, 2, ~purrr::pluck(.x, "scoreboard" ,"0", "matchups"))
    df <-
      preprocess %>%
      purrr::map_df(.league_resource_parse_fn,
                    list("league", 2, "scoreboard" , "0", "matchups"),
                    function(x)
                      purrr::map_df(x, .matchup_parse_fn))

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
        "matchup_week", "matchup_week_start", "matchup_week_end", "matchup_status",
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
        "matchup_team_managers_manager_is_commissioner", "matchup_team_managers_manager_manager_id_2",
        "matchup_team_managers_manager_nickname_2", "matchup_team_managers_manager_guid_2",
        "matchup_team_managers_manager_is_commissioner_2", "matchup_team_managers_manager_is_comanager",
        "matchup_team_managers_manager_felo_score_2", "matchup_team_managers_manager_felo_tier_2"
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
##                          TWO LEAGUES WITH 2 WEEKS                        ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Two leagues with two weeks arg uri returns valid response and is parsed to a tibble",{

    # Declare desired mock uri.
  mock_uri <-
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239,411.l.1240/scoreboard;type=week;week=1?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239,411.l.1240/scoreboard;type=week;week=2?format=json")

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

    # Subset good elements and pluck scoreboard data.
    preprocess <-
      preprocess[good_elements]

    # purrr::map_depth(2, purrr::map_at, 2, ~purrr::pluck(.x, "scoreboard" ,"0", "matchups"))
    df <-
      preprocess %>%
      purrr::map_df(.league_resource_parse_fn,
                    list("league", 2, "scoreboard" , "0", "matchups"),
                    function(x)
                      purrr::map_df(x, .matchup_parse_fn))

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
        "matchup_week", "matchup_week_start", "matchup_week_end", "matchup_status",
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
        "matchup_team_managers_manager_is_commissioner", "matchup_team_managers_manager_manager_id_2",
        "matchup_team_managers_manager_nickname_2", "matchup_team_managers_manager_guid_2",
        "matchup_team_managers_manager_is_commissioner_2", "matchup_team_managers_manager_is_comanager",
        "matchup_team_managers_manager_felo_score_2", "matchup_team_managers_manager_felo_tier_2"
      )

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)
})

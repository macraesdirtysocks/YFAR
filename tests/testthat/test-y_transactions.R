library(YFAR)

test_that("y_transactions uri generates properly",{

    y_transactions_uri_gen_test_fn <- function(key = NULL, count = NULL, transaction_type = NULL){

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
        subresource <- "transactions"
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

        uri_paths <-
            .uri_path_packer(key, 25)

        uri_parsed$params <-
            stringr::str_c(uri_out, uri_paths, "/", subresource, sep = "")

        if(!is.null(count)){
            # Sequence page numbers getting 25 each time.
            count_param_sequenced <- .seq_pages_fn(start = 0, count = count, i = 25)
            # count_param <- stringr::str_c(count_param_sequenced, sep = "=")
            uri_parsed$params <- stringr::str_c(uri_parsed$params, count_param_sequenced, sep = ";")
        }

        if(vctrs::vec_size(transaction_type) == 1){
            # Single transaction type (type =)
            transaction_type_param <- stringr::str_c("type", transaction_type, sep = "=")
            uri_parsed$params <- stringr::str_c(uri_parsed$params, transaction_type_param, sep = ";")
        } else if(vctrs::vec_size(transaction_type) > 1){
            # Multiple transaction types (types =)
            transaction_type_collapsed <- stringr::str_flatten(transaction_type, collapse = ",")
            transaction_type_param <- stringr::str_c("types", transaction_type_collapsed, sep = "=")
            uri_parsed$params <- stringr::str_c(uri_parsed$params, transaction_type_param, sep = ";")
        }

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
        y_transactions_uri_gen_test_fn("411.l.1239"),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/transactions?format=json")

    expect_identical(
        y_transactions_uri_gen_test_fn("411.l.1239", count = 5, transaction_type = "trade"),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/transactions;start=0;count=5;type=trade?format=json")

    expect_identical(
        y_transactions_uri_gen_test_fn("411.l.1239", count = 10, transaction_type = "add"),
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/transactions;start=0;count=10;type=add?format=json")

    expect_identical(
        y_transactions_uri_gen_test_fn("411.l.1239.t.8", count = 10, transaction_type = "add"),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8/transactions;start=0;count=10;type=add?format=json")

    expect_identical(
        y_transactions_uri_gen_test_fn(c("411.l.1239.t.8", "411.l.1239.t.1"), count = 2, transaction_type = "trade"),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1239.t.1/transactions;start=0;count=2;type=trade?format=json")

    expect_identical(
        y_transactions_uri_gen_test_fn(c("411.l.1239.t.8", "411.l.1239.t.1", "411.l.1239"), count = 2, transaction_type = "trade"),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1239.t.1/transactions;start=0;count=2;type=trade?format=json")

    expect_identical(
        y_transactions_uri_gen_test_fn(c("411.l.1239", "411.l.1239", "411.l.1239.t.8", "411.l.1239.t.1")),
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.8,411.l.1239.t.1/transactions?format=json")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          LEAGUE TRANSACTION PARSE                        ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Leagues uri returns valid response and is parsed to a tibble",{

    # Declare desired mock uri.
    mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239/transactions?format=json"

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

    # Define parse function relative to resource value.
    resource_parse_fn <-
        switch(resource,
               "leagues" = {.league_resource_parse_fn},
               "teams" = {.team_resource_parse_fn}
        )

    # Preprocess parsed content.
    preprocess <-
        r_parsed %>%
        purrr::flatten()

    # df
    df <-
        purrr::map_df(
            preprocess,
            resource_parse_fn,
            .transaction_parse_fn,
            .player_resource_parse_fn,
            .two_deep_subresource_parse
        ) %>%
        # Mutate time stamp from seconds to date.
        dplyr::mutate(
            transaction_timestamp = as.numeric(transaction_timestamp) %>%
                as.POSIXct(origin = "1970-01-01")
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
          "transaction_key", "transaction_id", "transaction_type", "transaction_status",
          "transaction_timestamp", "player_key", "player_id", "player_full",
          "player_first", "player_last", "player_ascii_first", "player_ascii_last",
          "player_editorial_team_abbr", "player_display_position", "player_position_type",
          "transaction_data_type", "transaction_data_source_type", "transaction_data_destination_type",
          "transaction_data_destination_team_key", "transaction_data_destination_team_name",
          "transaction_data_source_team_key", "transaction_data_source_team_name",
          "transaction_faab_bid")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                           TEAM TRANSACTION PARSE                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

testthat::test_that("Leagues uri returns valid response and is parsed to a tibble",{

    # Declare desired mock uri.
    mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/teams;411.l.1239.t.1/transactions?format=json"

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

    # Define parse function relative to resource value.

    # Resource parse function dependent on resource.
    resource_parse_fn <-
        switch(resource,
               "leagues" = {.league_resource_parse_fn},
               "teams" = {.team_resource_parse_fn}
        )

    # Preprocess parsed content.
    preprocess <-
        r_parsed %>%
        purrr::flatten()

    # df
    df <-
        purrr::map_df(
            preprocess,
            resource_parse_fn,
            .transaction_parse_fn,
            .player_resource_parse_fn,
            .two_deep_subresource_parse
        ) %>%
        # Mutate time stamp from seconds to date.
        dplyr::mutate(
            transaction_timestamp = as.numeric(transaction_timestamp) %>%
                as.POSIXct(origin = "1970-01-01")
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
          "team_manager_felo_tier", "transaction_key", "transaction_id",
          "transaction_type", "transaction_status", "transaction_timestamp",
          "player_key", "player_id", "player_full", "player_first", "player_last",
          "player_ascii_first", "player_ascii_last", "player_editorial_team_abbr",
          "player_display_position", "player_position_type", "transaction_data_type",
          "transaction_data_source_type", "transaction_data_destination_type",
          "transaction_data_destination_team_key", "transaction_data_destination_team_name",
          "transaction_data_source_team_key", "transaction_data_source_team_name",
          "transaction_faab_bid")


    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)
})



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            TEAM COUNT TYPE PARSE                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



testthat::test_that("Leagues uri returns valid response and is parsed to a tibble",{

    # Declare desired mock uri.
    mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1240/transactions;start=0;count=2;type=trade?format=json"

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

    # Define parse function relative to resource value.

    # Resource parse function dependent on resource.
    resource_parse_fn <-
        switch(resource,
               "leagues" = {.league_resource_parse_fn},
               "teams" = {.team_resource_parse_fn}
        )

    # Preprocess parsed content.
    preprocess <-
        r_parsed %>%
        purrr::flatten()

    # df
    df <-
        purrr::map_df(
            preprocess,
            resource_parse_fn,
            .transaction_parse_fn,
            .player_resource_parse_fn,
            .two_deep_subresource_parse
        ) %>%
        # Mutate time stamp from seconds to date.
        dplyr::mutate(
            transaction_timestamp = as.numeric(transaction_timestamp) %>%
                as.POSIXct(origin = "1970-01-01")
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
          "transaction_key", "transaction_id", "transaction_type", "transaction_status",
          "transaction_timestamp", "transaction_trader_team_key", "transaction_trader_team_name",
          "transaction_tradee_team_key", "transaction_tradee_team_name",
          "player_key", "player_id", "player_full", "player_first", "player_last",
          "player_ascii_first", "player_ascii_last", "player_editorial_team_abbr",
          "player_display_position", "player_position_type", "transaction_data_type",
          "transaction_data_source_type", "transaction_data_source_team_key",
          "transaction_data_source_team_name", "transaction_data_destination_type",
          "transaction_data_destination_team_key", "transaction_data_destination_team_name"
        )


    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)
})

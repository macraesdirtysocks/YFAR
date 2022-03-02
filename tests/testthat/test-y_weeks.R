library(YFAR)

test_that("y_transactions uri generates properly",{

    y_weeks_uri_gen_test_fn <- function(game_key = NULL){

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        resource <- "games"
        subresource <- "game_weeks"
        uri_out <- "game_keys="

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Check if keys are type league, remove FALSE and duplicates.
        key <- .single_resource_key_check(game_key, .game_key_check)

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
            stringr::str_c(uri_out, key_path, "/", subresource, sep = "")

        uri <- httr::build_url(uri_parsed)
    }


    expect_identical(
        y_weeks_uri_gen_test_fn("411"),
        "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/game_weeks?format=json")

    expect_identical(
        y_weeks_uri_gen_test_fn(c("411", "406")),
        "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,406/game_weeks?format=json")

    expect_identical(
        y_weeks_uri_gen_test_fn(c("411", "406", "411.l.1239")),
        "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,406/game_weeks?format=json")

    expect_identical(
        y_weeks_uri_gen_test_fn(c("411", "406", "411.l.1239", "411.l.1239", "411.l.1240")),
        "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,406/game_weeks?format=json")

    expect_identical(
        y_weeks_uri_gen_test_fn(c("411", "406", "411.l.1239", "411.l.1239", "411.l.1240")),
        "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,406/game_weeks?format=json")

    expect_error(
        y_weeks_uri_gen_test_fn()
        )

})



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                GAMES PARSE                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("y_weeks response is valid and parsed to a tibble",{

    # Declare desired mock uri.
    mock_uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/games;411/game_weeks?format=json"

    # Expected resource
    resource <-
        "games"

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

    # General list pre-processing
    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        list_pre_process_fn()

    # df
    df <-
        preprocess %>%
        purrr::map_df(
            .game_resource_parse_fn,
            pluck_args = list("game", 2, 1),
            fn = function(x)
                purrr::map_df(x, purrr::flatten_df)
        ) %>%
        dplyr::mutate(matchup_length = difftime(end, start))

    # Test that a tibble was returned from parsing.
    expect_true(tibble::is_tibble(df), TRUE)

    # Expected colnames.
    expected_colnames <-
        c("week", "display_name", "start", "end", "game_key", "game_id",
          "game_name", "game_code", "game_type", "game_url", "game_season",
          "game_is_registration_over", "game_is_game_over", "game_is_offseason",
          "matchup_length")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)

})

library(YFAR)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  TEST URI                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("y_stats_categories uri generates properly",{

y_stats_categories_uri_gen_test_fn <- function(game_key = NULL){


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    resource <- "games"
    uri_out <- "game_keys="

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    CHECKS                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Check if keys are type league, remove FALSE and duplicates.
    key <- .single_resource_key_check(game_key, .game_key_check)

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

    # Pack uri paths into lengths of 25
    key_path <- .uri_path_packer(key, 25)

    # Param base.
    uri_parsed$params <- stringr::str_c(uri_out, key_path, "/", c("stat_categories", "advanced_stat_categories"), sep = "")

    uri <- httr::build_url(uri_parsed)

}

    expect_identical(
        y_stats_categories_uri_gen_test_fn("411"),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/stat_categories?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/advanced_stat_categories?format=json")
        )

    expect_identical(
        y_stats_categories_uri_gen_test_fn(c("411", "403", "399")),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,403,399/stat_categories?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,403,399/advanced_stat_categories?format=json")
    )

})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            TEST RESPONSE PARSE                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



testthat::test_that("Response returns valid response and is parsed to a tibble",{

        # Declare desired uri.
        uri <-
            c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,403,399/stat_categories?format=json",
              "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,403,399/advanced_stat_categories?format=json")

        # Expected resource
        resource <-
            "games"

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
            purrr::transpose()

        df <- suppressWarnings(purrr::map_df(preprocess, .game_stats_category_parse_fn))

        # Test that a tibble was returned from parsing.
        expect_true(tibble::is_tibble(df), TRUE)

        # Expected colnames.
        expected_colnames <-
            c("game_key", "stats", "advanced_stats")

        # Test df colnames
        expect_named(df,
                     expected_colnames,
                     ignore.order = TRUE,
                     ignore.case = TRUE)

    })

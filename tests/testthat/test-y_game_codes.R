library(YFAR)

test_that("y_game_codes uri generates_properly", {
y_game_codes_uri_gen_fn <- function(sport = NULL) {

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    resource <- "games"

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                     URI                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Check provided sport names are in valid sports and take unique sports so redundant requests are not sent.
    sport <- sport[sport %in% c("mlb", "nba", "nhl", "nfl")] %>%
        vctrs::vec_unique()

    if(vctrs::vec_size(sport) >0) {
        sport <- glue::glue_collapse(sport, sep = ",")
    } else{
        stop(message(crayon::cyan("No valid sports provided.")), call. = FALSE)
    }

    uri <-
        glue::glue("https://fantasysports.yahooapis.com/fantasy/v2/games;game_codes={sport}?format=json")
}

expect_identical(
    y_game_codes_uri_gen_fn("nfl"),
    "https://fantasysports.yahooapis.com/fantasy/v2/games;game_codes=nfl?format=json"
)

expect_identical(
    y_game_codes_uri_gen_fn(c("nfl", "nhl")),
    "https://fantasysports.yahooapis.com/fantasy/v2/games;game_codes=nfl,nhl?format=json"
)

expect_identical(
    y_game_codes_uri_gen_fn(c("nfl", "nhl", "blah")),
    "https://fantasysports.yahooapis.com/fantasy/v2/games;game_codes=nfl,nhl?format=json"
)

})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              SINGLE GAME CODE                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

testthat::test_that("One game resource is parsed to a tibble",{

    # Declare desired uri.
    uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/games;game_codes=nfl?format=json"


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
    resource_parse_fn <-
        switch(resource,
               "games" = {
                   .game_resource_parse_fn
               },
               "leagues" = {
                   .league_resource_parse_fn
               })
    # Preprocess list.  This step is verbatim from y_matchups.
    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        purrr::keep(purrr::is_list)

    # DF
    df <-
        preprocess %>%
        purrr::map_df(.game_meta_parse_fn)

    # Test that a tibble was returned from parsing.
    expect_true(tibble::is_tibble(df), TRUE)


    # Expected colnames.
    expected_colnames <-
        c("game_key", "game_id", "game_name", "game_code", "game_type",
          "game_url", "game_season", "game_is_registration_over", "game_is_game_over",
          "game_is_offseason")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)

})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               TWO GAME CODES                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



testthat::test_that("Two game resources are parsed to a tibble",{

    # Declare desired uri.
    uri <-
        "https://fantasysports.yahooapis.com/fantasy/v2/games;game_codes=nfl,nhl?format=json"


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
    resource_parse_fn <-
        switch(resource,
               "games" = {
                   .game_resource_parse_fn
               },
               "leagues" = {
                   .league_resource_parse_fn
               })
    # Preprocess list.  This step is verbatim from y_matchups.
    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        purrr::keep(purrr::is_list)

    # DF
    df <-
        preprocess %>%
        purrr::map_df(.game_meta_parse_fn)

    # Test that a tibble was returned from parsing.
    expect_true(tibble::is_tibble(df), TRUE)


    # Expected colnames.
    expected_colnames <-
        c("game_key", "game_id", "game_name", "game_code", "game_type",
          "game_url", "game_season", "game_is_registration_over", "game_is_game_over",
          "game_is_offseason")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)

})

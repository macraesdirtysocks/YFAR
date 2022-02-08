library(YFAR)

test_that("uri is generated properly",{

y_standings__uri_gen_test_fn <- function(team_key = NULL) {

    # Check if keys are type league, remove FALSE and duplicates.
    key <- .single_resource_key_check(team_key, .team_key_check)

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    resource <- "teams"
    subresource <- "standings"
    uri_out <- "team_keys="

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

    key_paths <-
        .uri_path_packer(key, 25)

    uri_parsed$params <-
        stringr::str_c(uri_out, key_paths, "/", subresource, sep = "")

    uri <- httr::build_url(uri_parsed)
}

expect_identical(
    y_standings__uri_gen_test_fn("411.l.1239.t.1"),
    "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1/standings?format=json")

expect_identical(
    y_standings__uri_gen_test_fn(c("411.l.1239.t.1", "411.l.1239.t.8", "411.l.1239.t.5")),
    "https://fantasysports.yahooapis.com/fantasy/v2/teams;team_keys=411.l.1239.t.1,411.l.1239.t.8,411.l.1239.t.5/standings?format=json")


})


testthat::test_that("Response returns valid response and is parsed to a tibble",{
with_mock_api({

        # Declare desired mock uri.
        mock_uri <-
            "https://fantasysports.yahooapis.com/fantasy/v2/teams;411.l.1239.t.1,411.l.1239.t.8,411.l.1239.t.5/standings?format=json"

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


        # Preprocess list.  This step is verbatim from y_matchups.
        preprocess <-
            r_parsed %>%
            purrr::flatten() %>%
            purrr::keep(purrr::is_list)

        subresource_parse_fn <- function(x) {
            x %>%
                purrr::flatten_df() %>%
                janitor::clean_names()
        }

        df <-
            purrr::map_df(preprocess,
                          .team_resource_parse_fn,
                          subresource_parse_fn)

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
              "team_manager_felo_tier", "rank", "playoff_seed", "wins", "losses",
              "ties", "percentage")

        # Test df colnames
        expect_named(df,
                     expected_colnames,
                     ignore.order = TRUE,
                     ignore.case = TRUE)

})
})

context("Get transactions")
library(YFAR)

with_mock_api({
    testthat::test_that("request returns valid response and is parsed to a tibble",{

        resource <- "league"
        subresource <- "transactions"
        id <- "411.l.1239"
        count <- 1
        transaction_type <- "drop"


        # Build params

        params <-
            list(
                glue::glue("count={count}"),
                glue::glue("type={transaction_type}")
            ) %>%
            purrr::flatten_chr() %>%
            glue::glue_collapse(sep = ";")

        # testthat params is a string of length 1
        testthat::expect_identical(params, "count=1;type=drop")

        # Build uri
        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2", resource, id, subresource,sep = "/"),
                params = params,
                query = "format=json"
            )

        # testthat uri is built as expected
        testthat::expect_identical(uri, "https://fantasysports.yahooapis.com/fantasy/v2/league/411.l.1239/transactions;count=1;type=drop?format=json")


        r <- .y_get_response(uri = uri)

        # test response uri matches desired uri
        testthat::expect_identical(r$url, uri)

        # test that r is a response class
        testthat::expect_s3_class(r, class = "response")

        # test that response is json format
        testthat::expect_identical(httr::http_type(r), "application/json")

        # test that response is not an error
        testthat::expect_identical(httr::status_code(r), 200L)

        # get content
        r_parsed <-
            .y_parse_response(r, "fantasy_content", resource, 2, subresource)

        # parse function
        preprocess <-
            r_parsed %>%
            purrr::map(purrr::pluck, "transaction")

        df <-
            purrr::map_df(preprocess, .transaction_parse_fn) %>%
            dplyr::mutate(timestamp = as.numeric(timestamp) %>%
                              as.POSIXct(origin = "1970-01-01"))


        # test that df is a tibble
        testthat::expect_equal(tibble::is_tibble(df), TRUE)

        # expected colnames
        x <-
            c("transaction_key", "transaction_id", "type", "status", "timestamp",
              "faab_bid", "player_key", "player_id", "name_full", "name_first",
              "name_last", "name_ascii_first", "name_ascii_last", "editorial_team_abbr",
              "display_position", "position_type", "transaction_data_type",
              "transaction_data_source_type", "transaction_data_destination_type",
              "transaction_data_destination_team_key", "transaction_data_destination_team_name",
              "transaction_data_source_team_key", "transaction_data_source_team_name"
            )


        # test that colnames of the df match expected
        testthat::expect_named(df,
                               x,
                               ignore.order = TRUE,
                               ignore.case = TRUE)




    })
})


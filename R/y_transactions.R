#' Get transaction data from Yahoo! Fantasy API
#'
#' @param id league id or team id as a string in the form "000.l.0000" or "000.l.0000.t.0".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param count Number of transactions to return.
#' @param transaction_type Filter argument for request.  Accepts 1 of "commish", "add", "drop", "trade".
#' @param debug returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a list
#' @export
y_transactions <- function(id = NULL, token_name = NULL, count = NULL, transaction_type = NULL, debug = FALSE) {


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    api_token <- token_name
    resource <- .id_check(id)
    subresource <- "transactions"
    id <- id


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    CHECKS                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    .token_check(token_name, api_token, name = .GlobalEnv)
    stopifnot(!is.null(id))
    stopifnot(!is.null(count) | count >= 0)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                     URI                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    params <-
        list(
            glue::glue("count={count}"),
            glue::glue("type={transaction_type}")
            ) %>%
        purrr::flatten_chr() %>%
        glue::glue_collapse(sep = ";")


    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste(
                "fantasy/v2",
                resource,
                id,
                subresource,
                sep = "/"
            ),
            params = params,
            query = "format=json"
        )


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                GET RESPONSE                              ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    r <-
        .y_get_response(uri, api_token)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                   CONTENT                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    r_parsed <-
        .y_parse_response(r, "fantasy_content", resource, 2, subresource)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                PARSE CONTENT                             ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      DF                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    if(!debug){

        preprocess <-
            r_parsed %>%
            purrr::map(purrr::pluck, "transaction")

        df <-
            purrr::map_df(preprocess, .transaction_parse_fn) %>%
            dplyr::mutate(timestamp = as.numeric(timestamp) %>%
                              as.POSIXct(origin = "1970-01-01"))

    return(df)
    }


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    RETURN                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    data_list <-
        structure(
            list(
                response = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api"
        )

    return(data_list)

}

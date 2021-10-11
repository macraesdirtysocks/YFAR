#' Get transaction data from Yahoo! Fantasy API
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
y_transactions <- function(league_id = NULL, token_name = NULL) {

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    uri <-
        stringr::str_c(
            "https://fantasysports.yahooapis.com/fantasy/v2/league",
            league_id,
            "transactions?format=json",
            sep = "/"
        )

    r <-
        .y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "transactions")

    df <- purrr::map_df(r_parsed, .transactions_func)

    return(df)

}

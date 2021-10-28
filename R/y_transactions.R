#' Get transaction data from Yahoo! Fantasy API
#'
#' @param id league id or team id as a string in the form "000.l.0000" or "000.l.0000.t.0".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a list
#' @export
y_transactions <- function(id = NULL, token_name = NULL) {

    api_token <- token_name
    stopifnot(!is.null(id))
    .token_check(token_name, api_token, name = .GlobalEnv)


    #..........................API ARGUMENTS.........................


    resource <- .id_check(id)
    subresource <- "transactions"
    id <- id

    #..............................URI...............................

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
            query = "format=json"
        )

    #............................RESPONSE............................


    r <-
        .y_get_response(uri, api_token)


    #.........................PARSE RESPONSE.........................


    r_parsed <-
        .y_parse_response(r, "fantasy_content", resource)


    #......................INITIALIZE EMPTY LIST.....................


    transaction_data_list <-
        list(
            transaction_meta = NULL,
            player_data = NULL,
            transaction_type = NULL
        )

    #......................TRANSACTION META DATA.....................


    transaction_data_list$transaction_meta <-
        r_parsed %>%
        purrr::pluck(2, "transactions") %>%
        purrr::map(purrr::pluck, "transaction", 1) %>%
        purrr::map_df(dplyr::bind_cols, .id = "id") %>%
        dplyr::rename("transaction_type" = type)


    #..........................PLAYER DATA...........................

    transaction_data_list$player_data <-
        r_parsed %>%
        purrr::pluck(2, "transactions") %>%
        purrr::map(purrr::pluck, "transaction", 2, "players") %>%
        purrr::map_depth(2, purrr::pluck, "player", 1) %>%
        purrr::map_depth(1, purrr::keep, purrr::is_list) %>%
        purrr::map_depth(2, purrr::flatten) %>%
        purrr::map_depth(1, purrr::map_if, purrr::is_list, unlist) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::map_df(dplyr::bind_rows, .id = "id")


    #........................TRANSACTION TYPE........................

    transaction_data_list$transaction_type <-
        r_parsed %>%
        purrr::pluck(2, "transactions") %>%
        purrr::map(purrr::pluck, "transaction", 2, "players") %>%
        purrr::map_depth(2, purrr::pluck, "player", 2, "transaction_data") %>%
        purrr::map_depth(1, purrr::keep, purrr::is_list) %>%
        purrr::map_depth(2, purrr::flatten) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::map_df(dplyr::bind_rows)


    #...............................DF...............................

    df <-
        dplyr::left_join(
            transaction_data_list$transaction_meta,
            dplyr::bind_cols(
                transaction_data_list$player_data,
                transaction_data_list$transaction_type
            ),
            by = "id"
        ) %>%
        dplyr::select(-id) %>%
        dplyr::mutate(timestamp = as.numeric(timestamp) %>%
                          as.POSIXct(origin = "1970-01-01"))

    #.............................RETURN.............................


    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                df = df
            ),
            class = "yahoo_fantasy_api"
        )

    return(data_list)

}

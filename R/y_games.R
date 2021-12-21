#' Get all Yahoo! Fantasy hockey meta data.
#'
#' Returns a tibble containing all the meta data for all fantasy hockey
#' games and leagues participated in by the logged in user including those which
#' are currently active.
#'
#' Function is memoised
#'
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param quiet Toggle function messages.
#' @param debug returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a tibble
#' @export
y_games <-
    memoise::memoise(function(token_name = NULL, quiet = FALSE, debug = FALSE) {
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        resource <- "users"
        subresource1 <- "games"
        subresource2 <- "leagues"
        api_token <- token_name

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        .token_check(token_name, api_token, name = .GlobalEnv)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2", resource, sep = "/"),
                params = paste("use_login=1", subresource1, subresource2, sep = "/"),
                query = "format=json"
            )

        if(!quiet){cat("uri generated\n", uri, "\n")}

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        if(!quiet){print("Sending GET request")}

        r <-
            .y_get_response(uri, api_token)

        if(identical(httr::status_code(r), 401)){

            api_token$refresh()
            r <- httr::rerequest(r)
            return(r)
        }

        if(!quiet){
            print(paste("Response recieved with code", httr::message_for_status(r), sep = " "))
        }

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!quiet){print("Getting content from response")}

        r_parsed <-
            .y_parse_response(r, "fantasy_content", "users", "0", "user", 2, "games")


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!debug) {

            preprocess <-
                r_parsed %>%
                purrr::map(purrr::pluck, "game") %>%
                purrr::keep(purrr::is_list) %>%
                purrr::compact() %>%
                purrr::keep(purrr::every, purrr::is_list)

            if(!quiet){print("Parsing content with .y_games_parse")}

            df <- purrr::map_df(preprocess, .y_games_parse)

            return(df)
        }


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    RETURN                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        data_list <-
            structure(list(
                response = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")

        return(data_list)

    })

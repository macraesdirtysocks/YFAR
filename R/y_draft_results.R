#' Get draft results
#'
#' @param id League id or team id as a string in the form "000.l.0000" or "000.l.0000.t.0".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a list or tibble
#' @export
y_draft_results <- function(id = NULL, token_name = NULL, debug = FALSE) {

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    resource <- .id_check(id)
    subresource <- ifelse(resource == "league", "teams/draftresults", "draftresults")
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
        path = paste("fantasy/v2",resource, id, subresource, sep = "/"),
        query = "format=json")


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                GET RESPONSE                              ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    r <-
        .y_get_response(uri, api_token)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                PARSE CONTENT                             ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    r_parsed <-
        .y_parse_response(r, "fantasy_content")


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      DF                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if(!debug){

        # test for resource and run necessary parse function (.league_parse_fn or .team_parse_fn)


        #..........................league parse..........................

        if(resource == "league"){

            preprocess <-
                r_parsed %>%
                purrr::pluck(resource, 2, "teams") %>%
                purrr::keep(purrr::is_list) %>%
                purrr::map(purrr::pluck, "team", 2, "draft_results") %>%
                purrr::map(purrr::keep, purrr::is_list)

            df <- purrr::map_df(preprocess, purrr::flatten_df)


            return(df)


        } else if(resource == "team"){

            preprocess <-
                r_parsed %>%
                purrr::pluck(resource, 2, "draft_results") %>%
                purrr::keep(purrr::is_list)


            df <- purrr::map_df(preprocess, purrr::flatten_df)

            return(df)

        } else

            stop(print("something went wrong, possibly a parse function error"))
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

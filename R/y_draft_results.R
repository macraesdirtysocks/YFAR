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

    api_token <- token_name

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    CHECKS                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    .token_check(token_name, api_token, name = .GlobalEnv)

    # the if statement below accounts for the response depending on the arguments
    # supplied. if team_id is supplied map is not necessary but if a league_id
    # is supplied map is necessary because you get a list for each team.

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      if                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ##~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ league id check  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~

    if (stringr::str_detect(id, pattern = "[:digit:]*(\\.l\\.[:digit:]*)$") == TRUE) {

        #............................ARGUMENTS...........................

    resource <- "league"
    subresource <- "draftresults"
    league_id <- id

    #..............................URI...............................

    uri <-
        httr::modify_url(
        url = "https://fantasysports.yahooapis.com",
        path = paste("fantasy/v2",resource, league_id, subresource, sep = "/"),
        query = "format=json")

    #..........................GET RESPONSE..........................

    r <-
        .y_get_response(uri, api_token)

    #............................CONTENT.............................

    r_parsed <-
        .y_parse_response(r, "fantasy_content", resource, 2, "draft_results")

    #...............................DF...............................

    if(!debug){

        df <-
            r_parsed %>%
            purrr::keep(purrr::is_list) %>%
            purrr::map_df(purrr::flatten_df)

        return(df)
    }

    #.............................RETURN.............................

    data_list <-
        structure(
            list(
                response = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")

    return(data_list)

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                   else if                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ##~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ team id check  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~

    } else if (stringr::str_detect(id, pattern = "[:digit:]*\\.l\\.[:digit:]*(\\.t\\.[:digit:])$") == TRUE) {


        #............................ARGUMENTS...........................


        resource <- "team"
        subresource <- "draftresults"
        team_id <- id

        #..............................URI...............................

        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2", resource, team_id, subresource, sep = "/"),
                query = "format=json"
            )


        #..........................GET RESPONSE..........................


        r <-
            .y_get_response(uri, api_token)


        #............................CONTENT.............................


        r_parsed <-
            .y_parse_response(r, "fantasy_content", resource, 2, "draft_results")


        #...............................DF...............................


        if(!debug){

            df <-
                r_parsed %>%
                purrr::keep(purrr::is_list) %>%
                purrr::map_df(purrr::flatten_df)

            return(df)

        }



        #.............................RETURN.............................

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

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    else                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    } else {

        stop(message("please supply a league_id or team_id"))

    }
}

# }

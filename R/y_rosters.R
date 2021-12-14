#' Get roster data for a Yahoo Fantasy League.
#'
#' Supply a team or league id to the function to retrieve roster data.  If a date
#' is supplied the roster data for that date will be returned.
#'
#' Function accepts a vector of dates and can be mapped over for multiple teams
#'
#' Function is memoised
#'
#' @param id League id or team id as a string in the form "000.l.0000" or "000.l.0000.t.0".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with `y_create_token()`.
#' @param game_date Date of fantasy season in form YYYY-MM-DD to return.  Default is null and will return current date.
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a list or tibble
#' @export
y_rosters <- memoise::memoise(

    function(id = NULL, token_name = NULL, game_date = NULL, debug = FALSE){

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        api_token <- token_name
        resource <- .id_check(id)
        subresource <- ifelse(resource == "league", "teams/roster", "roster")


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        .token_check(token_name, api_token, name = .GlobalEnv)
        purrr::map(game_date, .date_check)


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        uri <- httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = glue::glue("fantasy/v2", resource, id, subresource, .sep = "/"),
            param = glue::glue("date={game_date}"),
            query = "format=json"
        )


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        r <-
            purrr::map(uri, .y_get_response, yahoo_token)


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        r_parsed <-
            purrr::map(r, .y_parse_response, "fantasy_content")


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!debug){

            # test for resource and run necessary parse function (.league_parse_fn or .team_parse_fn)


            #..........................league parse..........................

            if(resource == "league"){

                df <- purrr::map_df(r_parsed, .league_resource_fn)

                return(df)

                #...........................team parse...........................

            } else if(resource == "team"){

                df <- purrr::map_df(r_parsed, .team_parse_fn)

                return(df)

            } else

                stop(print("something went wrong, possibly a parse function error"))
        }


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    RETURN                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # if debug == TRUE return response data

        data_list <-
            structure(list(
                response = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")

        return(data_list)
    }

)

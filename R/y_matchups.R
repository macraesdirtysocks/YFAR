#' Get matchup stats and results data from Yahoo! Fantasy API
#'
#' y_matchups() gets matchup stats and results for a given league and week.
#' It returns a list with the meta data as well as tibble containing the matchups data
#' in the element `matchups_data`.
#'
#' If the week argument is set to a fantasy week that has yet to occur the stats will all
#' show a value of 1 which the api returns as boolean value for is_tied.
#'
#' @param team_id as a string in the form "000.l.0000.t.00".  Team id can be found with y_teams().
#' @param token_name assigned object name used when creating token with y_create_token().
#' @param week the week of fantasy season to return. Default NULL will return all weeks of season.
#' @param debug returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a tibble if debug = FALSE or a list if debug = TRUE
#' @export
y_matchups <-
    function(team_id = NULL, token_name = NULL, week = NULL, debug = FALSE) {

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        resource <- "team"
        subresource <- "matchups"
        api_token <- token_name

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        .team_id_check(team_id)
        .token_check(token_name, api_token, name = .GlobalEnv)
        stopifnot(is.null(week) | is.numeric(week))

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2", resource, team_id, subresource, sep = "/"),
                param = glue::glue("weeks={week}"),
                query = "format=json"
            )

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        r <-
            purrr::map(uri, .y_get_response, api_token)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        r_parsed <-
            purrr::map(r, .y_parse_response, "fantasy_content")


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        #...........................preprocess...........................


        # preprocess list to feed into parsing function

        preprocess <-
            r_parsed %>%
            purrr::map(purrr::pluck, "team", 2, "matchups") %>%
            purrr::map_depth(2, purrr::pluck, "matchup") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::compact() %>%
            purrr::flatten()


        #........................subset preprocess.......................


        # future matchups that have yet to take place break function.  This subsets them out.

        preprocess <-
            preprocess[purrr::map_lgl(preprocess, purrr::negate(~purrr::has_element(.x, "preevent")))]


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!debug){

        df <-
            purrr::map_df(preprocess, .matchup_parse_fn)

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

    }

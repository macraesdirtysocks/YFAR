#' Get standings data from Yahoo! Fantasy API
#'
#' Get standings data, divison data playoff seed and team matchup results.
#' This function does not return individual category win data form H2H leagues.
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param debug returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a list
#' @export
y_standings <- function(league_id = NULL, token_name = NULL, debug = FALSE) {



    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    resource <- "league"
    subresource <- "standings"
    api_token <- token_name


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    CHECKS                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                     URI                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, league_id, subresource, sep = "/"),
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
        .y_parse_response(r, "fantasy_content", "league", 2, "standings", 1, "teams") %>%
        purrr::map(purrr::pluck, "team") %>%
        purrr::compact()


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  EMPTY LIST                              ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    standings_list <- list(
        team_meta = NULL,
        team_points = NULL,
        team_standings = NULL,
        outcomes = NULL
    )


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                PARSE CONTENT                             ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    #............................TEAM META...........................

    standings_list$team_meta <-
        r_parsed %>%
        purrr::map(purrr::pluck, 1) %>%
        purrr::map(magrittr::extract, 1:3) %>%
        purrr::map_df(dplyr::bind_cols)


    #..........................TEAM POINTS...........................


    standings_list$team_points <-
        r_parsed %>%
        purrr::map_df(purrr::pluck, 2, "team_points")


    #.........................TEAM STANDINGS.........................


    standings_list$team_standings <-
        r_parsed %>%
        purrr::map(purrr::pluck, 3, "team_standings") %>%
        purrr::map_df(purrr::keep, purrr::negate(purrr::is_list))


    #............................OUTCOMES............................


    standings_list$outcomes <-
        r_parsed %>%
        purrr::map(purrr::pluck, 3, "team_standings") %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(unlist) %>%
        purrr::map_depth(2, as.character, .ragged = TRUE) %>%
        purrr::map_df(dplyr::bind_rows)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      DF                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    df <-
        purrr::reduce(standings_list, dplyr::bind_cols)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    RETURN                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if(!debug){return(df)}

    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                data = df
            ),
            class = "yahoo_fantasy_api")

    return(data_list)

}

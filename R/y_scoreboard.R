#' Get scoreboard stats and results data from Yahoo! Fantasy API
#'
#' `y_scoreboard()` gets scoreboard stats and results for a given league and week.
#' It is similar to `y_matchups()` but takes a league_id as opposed to a team_id and
#' returns league wide matchup data.  This eliminates mapping over team_id's with `y_matchups()`
#' to get league matchup data.
#'
#' `y_scoreboard()` takes a league id and returns matchup data for all teams in requested week.
#' `y_matchups()` takes a team id and returns matchup data for requested weeks and team.
#'
#' @param league_id league as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param weeks The week of fantasy season to return. Default NULL will return current week of season.
#' @param debug Returns a list of data uri call and content.  Useful for debugging.
#'
#' @return a list or tibble
#' @export
y_scoreboard <- function(league_id = NULL, token_name = NULL, weeks = NULL, debug = FALSE) {


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    resource <- "league"
    subresource <- "scoreboard"
    api_token <- token_name


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    CHECKS                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)
    stopifnot(is.null(weeks) | is.numeric(weeks))


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                     URI                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, league_id, subresource, sep = "/"),
            param = glue::glue("week={weeks}"),
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
        purrr::map(r, .y_parse_response, "fantasy_content", "league", 2, "scoreboard")


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      DF                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    if(!debug){

        preprocess <-
            r_parsed %>%
            purrr::map(purrr::pluck, "0", "matchups") %>%
            purrr::map(purrr::keep, purrr::is_list) %>%
            purrr::compact() %>%
            purrr::flatten()

        df <-
            purrr::map_df(preprocess, .matchup_parse_fn, .id = "matchup")

        return(df)
    }


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    RETURN                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    data_list <-
        structure(
            list(
                response= r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")


    return(data_list)

}

#' Get roster data for a Yahoo Fantasy League.
#'
#' Supply a team or league id to the function to retrieve roster data.  If a date
#' is supplied the roster data for that date will be returned.
#'
#' @param id league id or team id as a string in the form "000.l.0000" or "000.l.0000.t.0".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name assigned object name used when creating token with `y_create_token()`.
#' @param date date of fantasy season in form YYYY-MM-DD to return.  Default is null and will return current date.
#'
#' @return a list
#' @export
y_rosters <- function(id = NULL, token_name = NULL, date = NULL){

    api_token <- token_name

    .token_check(token_name, api_token, name = .GlobalEnv)
    .date_check(date)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                                                            ~~
    ##                                  Y ROSTERS                               ----
    ##                                                                            ~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # the if statement below accounts for the response depending on the arguments
    # supplied. if team_id is supplied mapping is not necessary but if a league_id
    # is supplied mapping is necessary because you get a list for each team.

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      if                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ##~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  ~ league id check  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~

    if (stringr::str_detect(id, pattern = "[:digit:]*(\\.l\\.[:digit:]*)$") == TRUE) {

        #.......................RESOURCE VARIABLES.......................

        resource <- "league"
        subresource1 <- "teams"
        subresource2 <- "roster"
        league_id <- id

        #..............................URI...............................

        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste(
                    "fantasy/v2",
                    resource,
                    league_id,
                    subresource1,
                    subresource2,
                    sep = "/"
                ),
                param = glue::glue("date={date}"),
                query = "format=json"
            )

        #............................RESPONSE............................

        r <-
            .y_get_response(uri, api_token)

        r_parsed <-
            .y_parse_response(r, "fantasy_content", resource)

        df <-
            .league_resource_fn(r_parsed)

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

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   else if                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ##~~~~~~~~~~~~~~~~~~~~~~~
        ##  ~ team id check  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~

    } else if (stringr::str_detect(id, pattern = "[:digit:]*\\.l\\.[:digit:]*(\\.t\\.[:digit:])$") == TRUE) {


        #.......................RESOURCE VARIABLES.......................

        resource <- "team"
        subresource <- "roster"
        team_id <- id

        #..............................URI...............................

        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2", resource, team_id, subresource, sep = "/"),
                param = glue::glue("date={date}"),
                query = "format=json"
            )

        #............................RESPONSE............................

        r <-
            .y_get_response(uri, api_token)

        r_parsed <-
            .y_parse_response(r, "fantasy_content", resource)


        #.............................RETURN.............................

        df <-
            .team_parse_fn(r_parsed)

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

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    else                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    } else {

        stop(message("please supply a league_id or team_id"))

    }
}

#' Get specific league settings from Yahoo! Fantasy API.
#'
#' Each Yahoo! Fantasy league can have it's own specific settings and this function will
#' get them for you.
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a list
#' @export
y_league_settings <- function(league_id = NULL, token_name = NULL){

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    resource <- "league"
    subresource <- "settings"
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
        .y_parse_response(r, "fantasy_content", "league")

    #...................... league scoring type......................

    # Get league scoring type

    scoring_type <-
        r_parsed[[1]][["scoring_type"]]

    cat(league_id, "is a", scoring_type, "league, returning league settings\n")

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                PARSE CONTENT                             ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # decide which helper to run based on scoring type, "points" or "categories"
    # this is found in the response content league info > scoring type

    df <- if(scoring_type != "point") {

        .category_league_settings(r_parsed)

    } else if (scoring_type == "point") {

        .point_league_settings(r_parsed)
    }

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    RETURN                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                scoring_type = scoring_type,
                data = df
            ),
            class = "yahoo_fantasy_api")

    return(data_list)
}

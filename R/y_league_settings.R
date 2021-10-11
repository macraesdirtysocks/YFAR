#' Get specific league settings from Yahoo! Fantasy API.
#'
#' Each Yahoo! Fantasy league can have it's own specific settings and this function will
#' get them for you.
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a [tibble][tibble::tibble-package] with nested columns.
#' @export
y_league_settings <- function(league_id = NULL, token_name = NULL){

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    # stopifnot(!is.null(token_name) & janitor::describe_class(api_token) == "Token2.0, Token, R6")

    uri <-  stringr::str_c(
        "https://fantasysports.yahooapis.com/fantasy/v2/league",
        league_id,
        "settings?format=json",
        sep = "/"
    )

    r <- .y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <- .y_parse_response(r, "fantasy_content", "league")

    scoring_type <- r_parsed[[1]][["scoring_type"]] #get league scoring type

    cat(league_id, "is a", scoring_type, "league, returning league settings\n")

    # decide which helper to run based on scoring type, "points" or "categories"
    if(scoring_type != "point") {
        .category_league_settings(r_parsed)

    } else if (scoring_type == "point") {
        .point_league_settings(r_parsed)

    }
}

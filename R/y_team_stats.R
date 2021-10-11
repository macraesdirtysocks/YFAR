#' Get team level stats for scoring categories
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a [tibble][tibble::tibble-package] with nested columns
#' @export
y_team_stats <- function(league_id = NULL, token_name = NULL){

    api_token <- token_name

    stopifnot(!is.null(league_id) & is.character(league_id))
    stopifnot(!is.null(token_name) & janitor::describe_class(api_token) == "Token2.0, Token, R6")

    uri <-  stringr::str_c(
        "https://fantasysports.yahooapis.com/fantasy/v2/league",
        league_id,
        "teams",
        "stats?format=json",
        sep = "/"
    )

    stopifnot(token_check() == 1)

    r <- y_get_response(uri, api_token)

    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")

    r_parsed <- y_parse_response(r, "fantasy_content", "league", 2, "teams")

    df <- dplyr::bind_cols(
        purrr::map_df(r_parsed, team_meta_func, "team", 1),
        purrr::map_df(r_parsed, team_stats_func)
    )

    return(df)
}

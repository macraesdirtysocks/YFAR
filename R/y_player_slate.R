#' Get player data from Yahoo! Fantasy API
#'
#' Function returns a tibble containing info on all players in your leagues universe.
#' By default the players are sorts by "Actual Rank"
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param start Return start number
#' @param number_of_players Count of players to return
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @export
y_player_slate <- function(league_id = NULL, token_name = NULL, start = 0, number_of_players = 100) {

    api_token <- token_name

    stopifnot(!is.null(league_id) & is.character(league_id))
    stopifnot(!is.null(token_name) & janitor::describe_class(api_token) == "Token2.0, Token, R6")
    stopifnot(is.numeric(start) & is.numeric(number_of_players))

    stopifnot(token_check() == 1)

    pages <- seq(from = start, to = (start+number_of_players)-25, by = 25)

    uri <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/league/", league_id, "/players;status=ALL;start=", pages, ";count=25;sort=AR?format=json")

    players <- purrr::map_df(uri, player_slate_func, y = api_token)

    # REMOVE DUPLICATE PLAYERS
    df <- players %>%
        dplyr::group_by(player_id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()

    return(df)

}

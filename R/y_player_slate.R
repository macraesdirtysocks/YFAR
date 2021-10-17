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
#' @return a list
#'
#' @export
y_player_slate <- function(league_id = NULL, token_name = NULL, start = 0, number_of_players = 100) {

    resource <- "league"
    subresource <- "players"

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)

    # if numer of plyaers is set to 100 function will return 125. This corrects that.
    adjust_players <- number_of_players - 25

    pages <-
        seq(from = start, to = adjust_players, by = 25)

    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, league_id, subresource, sep = "/"),
            params = glue::glue("status=ALL;start={pages};count=25;sort=AR"),
            query = "format=json"
        )

    r_parsed <-
        purrr::map_df(uri, .player_slate_func, y = api_token)

    # REMOVE DUPLICATE PLAYERS
    df <-
        r_parsed %>%
        dplyr::group_by(player_id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()

    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                pages_get = pages,
                matchup_data = df
            ),
            class = "yahoo_fantasy_api")

    return(data_list)

}

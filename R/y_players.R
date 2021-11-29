#' Get player data from Yahoo! Fantasy API
#'
#' This function is not intended to get all players in a league.  To get all players in your league use the
#' function `y_player_slate()`.
#'
#' Function returns a tibble containing info on all players in your leagues universe.
#' By default the players are sorted by "Actual Rank"
#'
#' This function is intented to get a subset of players i.e. top 100 players sorted by AR (actual rank).
#' If you want all players in that exist in your leagues universe use `y_player_slate()`
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param start Return start number
#' @param number_of_players Count of players to return
#' @param ... arguments sort and status passed onto internal .uri_gen_func
#'
#' @return a list
#'
#' @export
y_players <-
    memoise::memoise(function(league_id = NULL, token_name = NULL,  start = 0, number_of_players = 100, ...) {

        resource <- "league"
        subresource <- "players"

        api_token <- token_name

        .league_id_check(league_id)
        .token_check(token_name, api_token, name = .GlobalEnv)

        uri <-
            .uri_gen_func(number_of_players,
                          start,
                          resp_len = 25,
                          resource,
                          league_id,
                          subresource,
                          ...)

        r <-
            purrr::map(uri, .y_get_response, yahoo_token)


        r_parsed <-
            purrr::map(r, .y_parse_response, "fantasy_content", resource, 2)


        df <-
            purrr::map_df(r_parsed, .player_parse_fn)

        # REMOVE DUPLICATE PLAYERS

        df <-
            df %>%
            dplyr::group_by(player_id) %>%
            dplyr::slice_head(n = 1) %>%
            dplyr::ungroup()

        data_list <-
            structure(list(
                content = r_parsed,
                uri = uri,
                data = df
            ),
            class = "yahoo_fantasy_api")

        return(data_list)

    })

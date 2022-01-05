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
#' @param number_of_players Number of players
#' @param start Where to start the uris i.e. 50
#' @param debug Returns a list of data uri call and content.  Useful for debugging.
#' @param ... URI filter arguments (as characters) passed onto internal .uri_gen_func.
#'
#' @return a list or tibble
#'
#' @export
y_players <-
    memoise::memoise(function(league_id = NULL, token_name = NULL,
                              start = 0, number_of_players = 100, debug = FALSE, ...) {


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        resource <- "league"
        subresource <- "players"
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
            .uri_gen_func(
                league_id = league_id,
                resource = resource,
                subresource = subresource,
                start = start,
                number_of_players = number_of_players, ...)

        if(debug){print(uri)}


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        r <-
            purrr::map(uri, .y_get_response, api_token)


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        r_parsed <-
            purrr::map(r, .y_parse_response, "fantasy_content", resource, 2, subresource)


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!debug){

            df <-
                r_parsed %>%
                purrr::flatten() %>%
                purrr::map(.player_meta_func, "player", 1) %>%
                # add in rank column which is not included in the response
                purrr::set_names(nm = seq_along(.)) %>%
                purrr::imap(~purrr::prepend(.x, list("rank" = .y))) %>%
                purrr::map_df(dplyr::bind_cols)

            #....................remove duplicate players....................

            df <-
                df %>%
                dplyr::group_by(player_id) %>%
                dplyr::slice_head(n = 1) %>%
                dplyr::ungroup()

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

    })

#' Get team rosters
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a list
#' @export
y_rosters <- function(league_id = NULL, token_name = NULL) {

    resource <- "league"
    subresource1 <- "teams"
    subresource2 <- "roster"

    api_token <- token_name

    .league_id_check(league_id)
    .token_check(token_name, api_token, name = .GlobalEnv)


    #..............................URI...............................


    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, league_id, subresource1, subresource2, sep = "/"),
            query = "format=json"
        )


    #..........................GET RESPONSE..........................


    r <-
        .y_get_response(uri, api_token)


    #.........................PARSE RESPONSE.........................


    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "teams")


    #...........................Empty list...........................


    roster_list <-
        list(
            # team_coverage_type = NULL, # roster coverage type,  decided to exclude.
            team_meta = NULL,
            player_data =
                list(
                    meta = NULL,
                    selected_position = NULL
                )
            # minimum_games_coverage = NULL # goalie coverage type, decided to exclude
        )


    #............................TEAM META...........................


    roster_list$team_meta <-
        r_parsed %>%
        purrr::map(purrr::pluck, 1, 1) %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(purrr::compact) %>%
        purrr::set_names(nm = seq_along(.)) %>%
        purrr::map(`[`, 1:3) %>%
        # use unlist to purrr::keep all team meta.  decided to just use first 3 elements
        # purrr::map(unlist) %>%
        purrr::map_df(dplyr::bind_cols)


    #..........................ROSTER META...........................

    # roster coverage type

    # roster_list$roster_meta <-
    #     r_parsed %>%
    #     purrr::map(purrr::pluck, "team", 2, "roster") %>%
    #     purrr::map(purrr::keep, is_bare_atomic) %>%
    #     purrr::map_df(dplyr::bind_rows)


    #..........................PLAYER META...........................


    roster_list$player_data$meta <-
        r_parsed %>%
        purrr::map(purrr::pluck, "team", 2, "roster", "0", "players") %>%
        purrr::map_depth(2, purrr::pluck, "player", 1) %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(purrr::compact) %>%
        purrr::map_depth(2, purrr::keep, purrr::is_list) %>%
        purrr::map_depth(2, purrr::compact) %>%
        purrr::map_depth(1, purrr::map, purrr::flatten) %>%
        purrr::map_depth(2, purrr::map_if, purrr::is_list, purrr::flatten) %>%
        purrr::map_depth(2, purrr::map_at, "eligible_positions", ~purrr::set_names(., nm = paste("eligible", names(.), seq_along(.), sep = "_"))) %>%
        # purrr::map_depth(2, purrr::map_if, purrr::is_list, unlist)
        purrr::map_depth(2, rlang::squash) %>%
        purrr::map_depth(2, purrr::map_if, purrr::negate(is.character), as.character) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::set_names(., nm = seq_along(.)) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows(.id = "team_id")


    #.....................PLAYER ROSTER POSITION.....................


    roster_list$player_data$selected_position <-
        r_parsed %>%
        purrr::map(purrr::pluck, "team", 2, "roster", "0", "players") %>%
        purrr::map_depth(2, purrr::pluck, "player", 2, "selected_position") %>%
        purrr::map_depth(3, dplyr::bind_rows) %>%
        purrr::map_depth(2, dplyr::bind_cols) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows()


    #..........................MINIMUM GAMES.........................

    # goalie coverage type

    # roster_list$minimum_games <-
    #     r_parsed %>%
    #     purrr::map(purrr::pluck, "team", 2, "roster", "minimum_games") %>%
    #     purrr::map_df(dplyr::bind_rows)


    #............................dplyr::bind DF.............................


    df <-
        roster_list$player_data$meta %>%
        dplyr::bind_cols(., roster_list$player_data$selected_position) %>%
        dplyr::right_join(roster_list$team_meta, ., by = "team_id")


    #.............................RETURN.............................


    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                data_list = roster_list,
                data = df
            ),
            class = "yahoo_fantasy_api")

    return(data_list)


}

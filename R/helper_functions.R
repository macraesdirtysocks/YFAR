##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            TOKEN CHECK FUNCTION                          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#..........................Token Check...........................


#' Token Check
#'
#' Checks supplied environment for a Token2.0 class.
#' Checks token_name argument is supplied.
#' Checks that the supplied token_name is class Token2.0.
#'
#' @param token_name assigned object name used when creating token with y_create_token().
#' @param api_token assigned value of token name to standardize token names within functions.
#' @param ... argument passed onto ls()
#' @keywords internal
.token_check <- function(token_name, api_token, ...) {

    stopifnot(.token_count(...) == 1)
    stopifnot(!is.null(token_name))
    stopifnot(janitor::describe_class(api_token) == "Token2.0, Token, R6")
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            TOKEN COUNT FUNCTION                          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#......................Token Count Function......................


#' Token Count
#' Function called by .token_check and y_create_token()
#' Function called by .token_check
#' Check environment for a Token2.0 class.
#'
#' @param ... environment name argument passed to ls()
#'
#' @keywords internal
.token_count <- function(...) {

    purrr::map(.x = ls(...), .f = get) %>%
        purrr::map_chr(.f = janitor::describe_class) %>%
        stringr::str_detect(pattern = "Token") %>%
        sum()
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              LEAGUE ID CHECK                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#........................LEAGUE ID CHECK.........................


#' league_id check
#'
#' Checks for presence of supplied league_id and validity
#'
#' @param league_id league_id supplied to y_function
#'
#' @keywords internal
.league_id_check <- function(league_id){

    if (is.null(league_id) == TRUE){
        stop(message("league_id argument required but not supplied"))
    } else if (is.character(league_id) == FALSE){
        stop(message("league_id should be a character string"))
    } else if (stringr::str_detect(league_id, pattern = "[:digit:]*\\.l\\.[:digit:]*") == FALSE){
        stop(message("league_id syntax should be 000.l.0000"))
    } else {
        invisible(league_id)
    }

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                TEAM ID CHECK                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#..........................TEAM ID CHECK.........................


#' TEAM_id check
#'
#' Checks for presence of supplied team_id and validity
#'
#' @param team_id team id supplied to y_function
#'
#' @keywords internal
.team_id_check <- function(team_id){

    if (is.null(team_id) == TRUE){
        stop(message("team_id argument required but not supplied"))
    } else if (!is.character(team_id) == TRUE){
        stop(message("team_id should be a character string"))
    } else if (stringr::str_detect(team_id, pattern = "[:digit:]*\\.l\\.[:digit:]{3,5}^") == TRUE){
        stop(message("league_id supplied, function takes team_id syntax 000.l.0000.t.0"))
    } else if (!stringr::str_detect(team_id, pattern = "[:digit:]*\\.l\\.[:digit:]*\\.t\\.[:digit:]") == TRUE){
        stop(message("function takes team_id syntax 000.l.0000.t.0"))
    } else {
        invisible(team_id)
    }
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Y_GET_RESPONSE FUNCTION                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#....................Y_GET_RESPONSE FUNCTION.....................


#' y_get_response
#'
#' Send GET request to YAHOO! api
#'
#' @param x league_id supplied to y function
#' @param y api_token value assign by y_create_token()
#'
#' @keywords internal
.y_get_response <- function(x, y) {
    httr::GET(url = x,
              httr::add_headers(
                  Authorization = stringr::str_c("Bearer",
                                                 y$credentials$access_token, sep = " ")
              ))
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Y_PARSE_RESPONSE FUNCTION                       ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#....................Y_PARSE_RESPONSE FUNCTION...................


#' y_parse_response
#'
#' Parse response from y_get_response()
#'
#' @param x league_id supplied to y function
#' @param ... arguments passed onto purrr::pluck
#'
#' @keywords internal
.y_parse_response <- function(x, ...){
    jsonlite::fromJSON(
        httr::content(x, as = "text", encoding = "utf-8"), simplifyVector = FALSE) %>%
        purrr::pluck(...) %>%
        purrr::keep(purrr::is_list)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                      CATEGORY LEAGUE SETTINGS FUNCTION                   ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#................CATEGORY LEAGUE SETTINGS FUNCTION...............


#' category_league_settings
#'
#' helper function called by y_league_settings()
#'
#' @param r_parsed variable created within y_league_settings by y_parse_response
#'
#' @keywords internal
.category_league_settings <- function(r_parsed){

    league_meta <-
        r_parsed %>%
        purrr::pluck(1) %>%
        dplyr::bind_rows() %>%
        tibble::add_column("info" = "league_meta", .before = 1) %>%
        tidyr::nest(data = !("info"))

    league_settings <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1) %>%
        purrr::keep(purrr::negate(purrr::is_list)) %>%
        dplyr::bind_rows() %>%
        tibble::add_column("info" = "league_settings", .before = 1) %>%
        tidyr::nest(data = !("info"))

    roster_positions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "roster_positions") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(~dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = everything(), as.character))) %>%
        tibble::add_column("info" = "roster_positions", .before = 1) %>%
        tidyr::nest(data = !("info"))

    stat_categories <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "stat_categories", "stats") %>%
        purrr::map(purrr::flatten) %>%
        purrr::transpose() %>%
        purrr::map_at("stat_position_types", purrr::map, purrr::pluck, 1, "stat_position_type", "position_type") %>%
        purrr::transpose() %>%
        purrr::map_df(dplyr::bind_rows) %>%
        tibble::add_column("info" = "stat_categories", .before = 1) %>%
        tidyr::nest(data = !("info"))

    divisions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "divisions") %>%
        purrr::map(purrr::pluck, "division") %>%
        purrr::map_df(dplyr::bind_rows) %>%
        tibble::add_column("info" = "divisons", .before = 1) %>%
        tidyr::nest(data = !("info"))

    df <-
        dplyr::bind_rows(
            league_meta,
            league_settings,
            roster_positions,
            stat_categories,
            divisions)

    return(df)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                       POINT LEAGUE SETTINGS FUNCTION                     ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#.................POINT LEAGUE SETTINGS FUNCTION.................


#' points_league_settings
#'
#' helper function called by y_league_settings()
#'
#' @param r_parsed variable created within y_league_settings by y_parse_response
#'
#' @keywords internal
.point_league_settings <- function(r_parsed) {

    league_meta <-
        r_parsed %>%
        purrr::pluck(1) %>%
        dplyr::bind_rows() %>%
        tibble::add_column("info" = "league_meta", .before = 1) %>%
        tidyr::nest(data = !("info"))

    league_settings <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1) %>%
        purrr::keep(purrr::negate(purrr::is_list)) %>%
        dplyr::bind_rows() %>%
        tibble::add_column("info" = "league_settings", .before = 1) %>%
        tidyr::nest(data = !("info"))

    roster_positions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "roster_positions") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(~dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = everything(), as.character))) %>%
        tibble::add_column("info" = "roster_positions", .before = 1) %>%
        tidyr::nest(data = !("info"))

    stat_categories <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "stat_categories", "stats") %>%
        purrr::map(purrr::flatten) %>%
        purrr::transpose() %>%
        purrr::map_at("stat_position_types", purrr::map, purrr::pluck, 1, "stat_position_type", "position_type") %>%
        purrr::transpose() %>%
        purrr::map_df(dplyr::bind_rows)

    stat_modifiers <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "stat_modifiers", "stats") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(dplyr::bind_rows)

    stats <-
        dplyr::left_join(stat_categories, stat_modifiers, by = "stat_id") %>%
        tibble::add_column("info" = "stat_modifiers", .before = 1) %>%
        dplyr::rename("modifier_value" = "value") %>%
        tidyr::nest(data = !("info"))

    df <-
        dplyr::bind_rows(
            league_meta,
            league_settings,
            roster_positions,
            stats)

    return(df)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          TEAM META DATA FUNCTION                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#....................TEAM META DATA FUNCTION.....................


#' team_meta_data
#'
#' helper function called by y_ functions to parse team meta data
#'
#' @param x parsed content from response object
#' @param ... arguments passed onto purrr::pluck
#'
#' @keywords internal
.team_meta_func <- function(x, ...) {
        team_info <-
            x %>%
            purrr::pluck(...) %>%
            purrr::compact() %>%
            purrr::flatten() %>%
            purrr::discard(purrr::is_list) %>%
            purrr::modify(as.character) %>%
            dplyr::bind_rows() %>%
            tidyr::nest(team_info = -c(team_key, team_id, name))

        team_logo <-
            x %>%
            purrr::pluck(...) %>%
            purrr::compact() %>%
            purrr::flatten() %>%
            purrr::pluck("team_logos") %>%
            rlang::squash() %>%
            dplyr::bind_rows() %>%
            tidyr::nest(team_logo = dplyr::everything())

        roster_adds <-
            x %>%
            purrr::pluck(...) %>%
            purrr::compact() %>%
            purrr::flatten() %>%
            purrr::pluck("roster_adds") %>%
            rlang::squash() %>%
            dplyr::bind_rows() %>%
            tidyr::nest(roster_adds = dplyr::everything())

        managers <-
            x %>%
            purrr::pluck(...) %>%
            purrr::compact() %>%
            purrr::flatten() %>%
            purrr::pluck("managers", 1, "manager") %>%
            rlang::squash() %>%
            dplyr::bind_rows() %>%
            tidyr::nest(manager_info = dplyr::everything())

        df <- dplyr::bind_cols(team_info, team_logo, roster_adds, managers)

    return(df)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            STATS DATA FUNCTION                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#......................STATS DATA FUNCTION.......................


#' stats_data_func
#'
#' helper function called by y_standings to parse team standings response
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.stats_data_func <- function(x) {

    stats_count <-
        x %>%
        purrr::pluck(2, "team_stats", "stats") %>%
        purrr::flatten() %>%
        purrr::modify_depth(2, as.character) %>%
        purrr::map_df(purrr::flatten_df) %>%
        tidyr::nest(stat_count = dplyr::everything())

    team_points <-
        x %>%
        purrr::pluck(2, "team_points") %>%
        purrr::modify_depth(1, as.character) %>%
        purrr::flatten_df()

    team_standings <-
        x %>%
        purrr::pluck(3, "team_standings") %>%
        purrr::discard(purrr::is_list) %>%
        purrr::modify_depth(1, as.character) %>%
        purrr::flatten_df()

    outcome_totals <-
        x %>%
        purrr::pluck(3, "team_standings", "outcome_totals") %>%
        purrr::modify_depth(1, as.character) %>%
        purrr::flatten_df() %>%
        tidyr::nest(outcome_totals = dplyr::everything())

    divison_outcome_totals <-
        x %>%
        purrr::pluck(3, "team_standings", "divisional_outcome_totals") %>%
        purrr::modify_depth(1, as.character) %>%
        purrr::flatten_df() %>%
        tidyr::nest(divisonal_outcome_totals = dplyr::everything())

    df <-
        dplyr::bind_cols(
            stats_count,
            team_points,
            team_standings,
            outcome_totals,
            divison_outcome_totals
        )

    return(df)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            TEAM STATS FUNCTION                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#......................TEAM STATS FUNCTION.......................


#' team_stats_func
#'
#' helper function called by y_team_stats to parse team stats response
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.team_stats_func <- function(x) {

    team_stats <-
        x %>%
        purrr::pluck("team", 2, "team_stats", "stats") %>%
        purrr::map_df(purrr::pluck, "stat") %>%
        tidyr::pivot_wider(
            names_from = stat_id,
            values_from = value,
            names_prefix = "stat_"
        )

    team_points <-
        x %>%
        purrr::pluck("team", 2, "team_points") %>%
        dplyr::bind_rows()

    df <- dplyr::bind_cols(team_points, team_stats)

    return(df)

}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               YAHOO MATCHUPS                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#.........................Team Info Func.........................


#' team_info_func
#'
#' helper function called by y_matchups to parse team_info from matchups response
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.team_info_func <- function(x) {
    df <- tibble::tibble(
        purrr::map_dfr(x, purrr::pluck, "team", 1, 1),
        purrr::map_dfr(x, purrr::pluck, "team", 1, 2),
        purrr::map_dfr(x, purrr::pluck, "team", 1, 3)
    ) %>%
        dplyr::rename("team" = "name")
}

#.........................Week Info Function.........................

#' week_info_func
#'
#' helper function called by y_matchups to parse week_info from matchups response
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.week_info_func <- function(x) {

    df <-
        x %>%
        purrr::set_names(x =., nm = purrr::map_chr(.x = ., purrr::pluck, "team", 1, 3, 1)) %>%
        purrr::map(purrr::pluck, "team", 2, "matchups") %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(purrr::map_depth, 2, purrr::keep, purrr::negate(purrr::is_list)) %>%
        purrr::map(purrr::map_depth, 1, purrr::flatten) %>%
        purrr::map(purrr::map_depth, 2, as.character) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::nest_by(team, .key = "week_info")

    return(df)
}

#......................Stat Winner Function......................

#' stats_winner_func
#'
#' helper function called by y_matchups to parse stat_winners from matchups response
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.stat_winner_func <- function(x) {

    df <-
        x %>%
        purrr::set_names(x = ., nm = purrr::map_chr(.x =., purrr::pluck, "team", 1, 3, 1)) %>%
        purrr::map(purrr::pluck, "team", 2, "matchups") %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(.x = ., ~purrr::set_names(x = ., nm = seq_along(.))) %>%
        purrr::map(purrr::map_depth, 2, purrr::pluck, "stat_winners") %>%
        purrr::map_depth(4, purrr::flatten) %>%
        purrr::map_depth(2, purrr::flatten_df) %>%
        purrr::map_depth(3, as.character) %>%
        purrr::map(dplyr::bind_rows, .id = "week") %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::nest_by(team, .key = "stat_winner")

    return(df)
}

#......................Week Stats Function.......................

#' week_stats_func
#'
#' helper function called by y_matchups to parse week stats from matchups response
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.week_stats_func <- function(x) {

    ARTofR::xxx_divider2("data")


    ARTofR::


    data <-
        x %>%
        purrr::set_names(x =., nm = purrr::map_chr(.x = ., purrr::pluck, "team", 1, 3, 1)) %>%
        purrr::map(purrr::pluck, "team", 2, "matchups") %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map_depth(3, purrr::pluck, "0", "teams") %>%
        purrr::map(.x = ., ~purrr::set_names(x =., nm = seq_along(.))) %>%
        purrr::map(purrr::map_depth, 4, purrr::map_at, 1, magrittr::extract, c(1:3)) %>%
        purrr::map(purrr::map_depth, 2, purrr::map_at, "1", purrr::pluck, "team", 1) %>%
        purrr::map(purrr::map_depth, 2, magrittr::extract, c(1:2))

    opponent <-
        data %>%
        purrr::map_depth(2, purrr::pluck, "matchup", "1") %>%
        purrr::map_depth(2, purrr::flatten) %>%
        purrr::map_depth(3, as.character) %>%
        purrr::map(dplyr::bind_rows, .id = "week") %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::nest_by(team, .key = "opponent_info")

    team_points <-
        data %>%
        purrr::map_depth(2, purrr::pluck, "matchup", "0", "team", 2, "team_points") %>%
        purrr::map_depth(2, magrittr::extract, "total") %>%
        purrr::map_depth(3, as.character) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::map(dplyr::bind_rows, .id = "week") %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::nest_by(team, .key = "team_points")

    remaining_games <-
        data %>%
        purrr::map_depth(2,
                         purrr::pluck,
                         "matchup",
                         "0",
                         "team",
                         2,
                         "team_remaining_games",
                         "total") %>%
        purrr::map_depth(3, as.character) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::map(dplyr::bind_rows, .id = "week") %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::nest_by(team, .key = "remaining_games")

    week_stats <-
        data %>%
        purrr::map_depth(2,
                         purrr::pluck,
                         "matchup",
                         "0",
                         "team",
                         2,
                         "team_stats",
                         "stats") %>%
        purrr::map_depth(3, purrr::pluck, "stat") %>%
        purrr::modify_depth(4, as.integer) %>%
        purrr::map_depth(3, dplyr::bind_rows) %>%
        purrr::map_depth(2, dplyr::bind_rows) %>%
        purrr::map(dplyr::bind_rows, .id = "week") %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::nest_by(team, .key = "week_stats")

    df <-
        dplyr::full_join(opponent, team_points, by = "team") %>%
        dplyr::full_join(remaining_games, by = "team") %>%
        dplyr::full_join(week_stats, by = "team")

    return(df)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        YAHOO! TRANSACTIONS FUNCTION                      ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#..................YAHOO! TRANSACTIONS FUNCTION..................


#' transactions_func
#'
#' helper function called by y_transactions to parse transaction response
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.transactions_func <- function(x){

    # ARTofR::xxx_divider1("TRANSACTION META DATA")

    #......................TRANSACTION META DATA.....................

    transaction_meta <-
        x %>%
        purrr::pluck("transaction", 1) %>%
        dplyr::bind_rows() %>%
        dplyr::rename("transaction_type" = "type") # rename so there isn't a conflict when binding cols

    #..........................PLAYER INFO...........................

    player_info <-
        x %>%
        purrr::pluck("transaction", 2, "players") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map(purrr::keep, purrr::is_list) %>% #function breaks without this
        purrr::flatten() %>%
        purrr::set_names(x =., nm = paste(names(.), seq_along(.), sep = "_")) %>% # set names to player_#
        purrr::map_depth(2, purrr::map_at, 3, purrr::map_at, "name", purrr::pluck, 1) %>%
        purrr::map(purrr::map_at, 2, purrr::map_at, "transaction_data", purrr::flatten) %>% #put player transaction data on the same level,
        # for some reason player 2 is on a different level.
        purrr::map_depth(2, purrr::flatten_df) %>%
        purrr::map_df(dplyr::bind_cols)

    df <- dplyr::bind_cols(transaction_meta, player_info)

    return(df)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        YAHOO! PLAYER SLATE FUNCTION                      ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#..................YAHOO! PLAYER SLATE FUNCTION..................


#' player_slate_func
#'
#' helper function called by y_player_slate to parse player info from players response
#'
#' @param x parsed content from response object
#' @param ... arguments passed onto purrr::pluck
#'
#' @keywords internal
.player_slate_func <- function(x, ...){


    #......................GET RESPONSE.....................

    r <-
        .y_get_response(x, ...)


    #......................PARSE RESPONSE.....................

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "players")


    #..........................PLAYER SLATE...........................

    df <-
        r_parsed %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::map_depth(2, purrr::flatten) %>%
        purrr::map(purrr::flatten) %>%
        purrr::map(purrr::flatten) %>%
        purrr::map(purrr::map_at, "name", purrr::pluck, "full") %>%
        purrr::map(purrr::map_at, "eligible_positions", purrr::flatten) %>%
        purrr::map(purrr::map_at, "eligible_positions", ~purrr::set_names(x =., nm = paste("position", seq_along(.), sep = "_"))) %>%
        purrr::map(~purrr::discard(.x =., names(.) == "headshot")) %>%
        purrr::map_df(purrr::flatten_df)

    return(df)
}




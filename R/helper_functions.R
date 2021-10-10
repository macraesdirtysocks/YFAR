
# ARTofR::xxx_title1("Token Check Function")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            TOKEN CHECK FUNCTION                          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Check for token class in .GLobalEnv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Check for token class in .GLobalEnv  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
token_check <- function(){
    purrr::map(.x = ls(name = .GlobalEnv), .f = get) %>%
        purrr::map_chr(.f = janitor::describe_class) %>%
        stringr::str_detect(pattern = "Token") %>%
        sum()
}

# ARTofR::xxx_title1("League ID Check")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              LEAGUE ID CHECK                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Verify structure of league_id argument")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Verify structure of league_id argument  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
league_id_check <- function(x){
    stopifnot(!is.null(x))
    stopifnot(is.character(x))
    stopifnot(stringr::str_detect(x, pattern = "[:digit:]*\\.l\\.[:digit:]*"))
}

# ARTofR::xxx_title1("y_get_response function")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Y_GET_RESPONSE FUNCTION                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Get response object from Yahoo! API")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Get response object from Yahoo! API  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
y_get_response <- function(x, y) {
    httr::GET(
        url = x,
        httr::add_headers(
                  Authorization = stringr::str_c(
                      "Bearer",
                      y$credentials$access_token, sep = " "
                      )
                  )
        )
}

# ARTofR::xxx_title1("Y_PARSE_RESPONSE FUNCTION")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Y_PARSE_RESPONSE FUNCTION                       ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Parse response object from Yahoo! API")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Parse response object from Yahoo! API  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
y_parse_response <- function(x, ...){
    jsonlite::fromJSON(
        httr::content(x, as = "text", encoding = "utf-8"), simplifyVector = FALSE) %>%
        purrr::pluck(...) %>%
        purrr::keep(purrr::is_list)
}

# ARTofR::xxx_title1("CATEGORY LEAGUE SETTINGS FUNCTION")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                      CATEGORY LEAGUE SETTINGS FUNCTION                   ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Parse settings from a Yahoo! Fantasy Category League.")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Parse settings from a Yahoo! Fantasy Category League.  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
category_league_settings <- function(r_parsed){

    league_meta <-
        r_parsed %>%
        purrr::pluck(1) %>%
        dplyr::bind_rows() %>%
        tibble::add_column(info = "league_meta", .before = 1) %>%
        tidyr::nest(data = !info)

    league_settings <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1) %>%
        purrr::keep(purrr::negate(purrr::is_list)) %>%
        dplyr::bind_rows() %>%
        tibble::add_column(info = "league_settings", .before = 1) %>%
        tidyr::nest(data = !info)

    roster_positions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "roster_positions") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(., ~dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = everything(), as.character))) %>%
        tibble::add_column(info = "roster_positions", .before = 1) %>%
        tidyr::nest(data = !info)

    stat_categories <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "stat_categories", "stats") %>%
        purrr::map(purrr::flatten) %>%
        purrr::transpose() %>%
        purrr::map_at("stat_position_types", purrr::map, purrr::pluck, 1, "stat_position_type", "position_type") %>%
        purrr::transpose() %>%
        purrr::map_df(dplyr::bind_rows) %>%
        tibble::add_column(info = "stat_categories", .before = 1) %>%
        tidyr::nest(data = !info)

    divisions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "divisions") %>%
        purrr::map(purrr::pluck, "division") %>%
        purrr::map_df(dplyr::bind_rows) %>%
        tibble::add_column(info = "divisons", .before = 1) %>%
        tidyr::nest(data = !info)

    df <-
        dplyr::bind_rows(
            league_meta,
            league_settings,
            roster_positions,
            stat_categories,
            divisions)

    return(df)
}

# ARTofR::xxx_title1("POINT LEAGUE SETTINGS FUNCTION")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                       POINT LEAGUE SETTINGS FUNCTION                     ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Parse settings from a Yahoo! Fantasy Point League.")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Parse settings from a Yahoo! Fantasy Point League.  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
point_league_settings <- function(r_parsed) {

    league_meta <-
        r_parsed %>%
        purrr::pluck(1) %>%
        dplyr::bind_rows() %>%
        tibble::add_column(info = "league_meta", .before = 1) %>%
        tidyr::nest(data = !info)

    league_settings <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1) %>%
        purrr::keep(purrr::negate(purrr::is_list)) %>%
        dplyr::bind_rows() %>%
        tibble::add_column(info = "league_settings", .before = 1) %>%
        tidyr::nest(data = !info)

    roster_positions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "roster_positions") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(., ~dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = everything(), as.character))) %>%
        tibble::add_column(info = "roster_positions", .before = 1) %>%
        tidyr::nest(data = !info)

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
        tibble::add_column(info = "stat_modifiers", .before = 1) %>%
        dplyr::rename(modifier_value = value) %>%
        tidyr::nest(data = !info)

    df <-
        dplyr::bind_rows(
            league_meta,
            league_settings,
            roster_positions,
            stats)

    return(df)
}

# ARTofR::xxx_title1("TEAM META DATA FUNCTION")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          TEAM META DATA FUNCTION                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Parse team meta data in standings response.")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Parse team meta data in standings response.  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
team_meta_func <- function(x, ...) {

        team_info <-
            x %>%
            purrr::pluck(...) %>%
            purrr::compact() %>%
            purrr::flatten() %>%
            purrr:: discard(purrr::is_list) %>%
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

# ARTofR::xxx_title1("STATS DATA FUNCTION")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            STATS DATA FUNCTION                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Parse team stats from standings response")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Parse team stats from standings response  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
stats_data_func <- function(x) {

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

# ARTofR::xxx_title1("TEAM STATS FUNCTION")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            TEAM STATS FUNCTION                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xxx_title3("Parse team stats")

##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Parse team stats  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
team_stats_func <- function(x) {

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

# xxx_title3("Parse YAHOO! Matchups Response")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Parse YAHOO! Matchups Response  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#.........................Team Info Func.........................

#' @export
team_info_func <- function(x) {
    df <- tibble::tibble(
        purrr::map_dfr(x, purrr::pluck, "team", 1, 1),
        purrr::map_dfr(x, purrr::pluck, "team", 1, 2),
        purrr::map_dfr(x, purrr::pluck, "team", 1, 3)
    ) %>%
        dplyr::rename(team = name)
}

# ARTofR::xxx_divider1("Week Info Function")

#.........................Week Info Function.........................

#' @export
week_info_func <- function(x) {

    df <-
        x %>%
        purrr::set_names(nm = purrr::map_chr(., purrr::pluck, "team", 1, 3, 1)) %>%
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

# ARTofR::xxx_divider1("Stat Winner Function")

#......................Stat Winner Function......................

#' @export
stat_winner_func <- function(x) {

    df <-
        x %>%
        purrr::set_names(nm = purrr::map_chr(., purrr::pluck, "team", 1, 3, 1)) %>%
        purrr::map(purrr::pluck, "team", 2, "matchups") %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(., ~ purrr::set_names(., nm = seq_along(.))) %>%
        purrr::map(purrr::map_depth, 2, purrr::pluck, "stat_winners") %>%
        purrr::map_depth(4, purrr::flatten) %>%
        purrr::map_depth(2, purrr::flatten_df) %>%
        purrr::map_depth(3, as.character) %>%
        purrr::map(dplyr::bind_rows, .id = "week") %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::nest_by(team, .key = "stat_winner")

    return(df)
}

# ARTofR::xxx_divider1("Week Stats Function")

#......................Week Stats Function.......................

#' @export
week_stats_func <- function(x) {

    data <-
        x %>%
        purrr::set_names(nm = purrr::map_chr(., purrr::pluck, "team", 1, 3, 1)) %>%
        purrr::map(purrr::pluck, "team", 2, "matchups") %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map_depth(3, purrr::pluck, "0", "teams") %>%
        purrr::map(., ~ purrr::set_names(., nm = seq_along(.))) %>%
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

# ARTofR::xxx_title1("YAHOO! TRANSACTIONS FUNCTION")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        YAHOO! TRANSACTIONS FUNCTION                      ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ARTofR::xxx_title3("PARSE TRANSACTIONS RESPONSE")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ PARSE TRANSACTIONS RESPONSE  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

transactions_func <- function(x){

    # ARTofR::xxx_divider1("TRANSACTION META DATA")

    #......................TRANSACTION META DATA.....................

    transaction_meta <-
        x %>%
        purrr::pluck("transaction", 1) %>%
        dplyr::bind_rows() %>%
        dplyr::rename(transaction_type = type) # rename so there isn't a conflict when binding cols

    # ARTofR::xxx_divider1("PLAYER INFO")

    #..........................PLAYER INFO...........................

    player_info <-
        x %>%
        purrr::pluck("transaction", 2, "players") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map(purrr::keep, purrr::is_list) %>% #function breaks without this
        purrr::flatten() %>%
        purrr::set_names(paste(names(.), seq_along(.), sep = "_")) %>% # set names to player_#
        purrr::map_depth(2, purrr::map_at, 3, purrr::map_at, "name", purrr::pluck, 1) %>%
        purrr::map(purrr::map_at, 2, purrr::map_at, "transaction_data", purrr::flatten) %>% #put player transaction data on the same level,
        # for some reason player 2 is on a different level.
        purrr::map_depth(2, purrr::flatten_df) %>%
        purrr::map_df(dplyr::bind_cols)

    df <- dplyr::bind_cols(transaction_meta, player_info)

    return(df)

}

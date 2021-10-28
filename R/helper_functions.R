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
##                                  ID CHECK                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' id check
#'
#' detect if league or team id provided to id argument
#'
#' @param id league_id supplied to y_function
#'
#' @keywords internal
.id_check <- function(id){

    if (stringr::str_detect(id, pattern = "[:digit:]*(\\.l\\.[:digit:]*)$") == TRUE) {
        "league"
    } else if (stringr::str_detect(id, pattern = "[:digit:]*\\.l\\.[:digit:]*(\\.t\\.[:digit:])$") == TRUE) {
        "team"
    } else {
        stop(message("please supply a league_id or team_id"))
    }
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
##                                 WEEK CHECK                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#...........................WEEK CHECK...........................


#' week check
#'
#' Checks for presence of supplied week and validity
#'
#' @param week team id supplied to y_function
#'
#' @keywords internal
.week_check <- function(week){

    stopifnot(!is.null(week))
    stopifnot(week != "current" | !is.numeric(week))
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
    r <- httr::GET(url = x,
              httr::add_headers(
                  Authorization = stringr::str_c("Bearer",
                                                 y$credentials$access_token, sep = " ")
              ))
    httr::stop_for_status(r, task = "authorize, refresh token with yahoo_token$refresh() and try again")
    stopifnot(httr::http_type(r) == "application/json")
    return(r)
}

ARTofR::xxx_title1("DATE CHECK FUNCTION")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            DATE CHECK FUNCTION                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check format of the date supplied to the date argument in y_rosters
# necessary form is "%Y-%m-%d"


#' check date format
#'
#' @param date
#'
#' @keywords internal
.date_check <- function(x) {
    if (is.null(x)) {
        x
    } else if (!is.na(as.Date(x, format = "%Y-%m-%d"))) {
        x
    } else {
        NULL
        message("please supply a valid date in format %Y-%m-%d, returning rosters for today's date")
    }
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
##                          Y ROSTER HELPER FUNCTIONS                       ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# These helper functions are called in succession depending on what level
# of the Yahoo Fantasy API is queried.

# League -> teams -> roster -> players

# A league is made up of teams, teams are made up of rosters and rosters
# are made up of players.

# Essentially what I attempted to do is standardize where each functions starts
# so when the function index into the and hit a particular element that element
# is then fed to the next parsing function.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            league resource parse                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' parse return from league resource
#'
#' helper function called by y_rosters.  this function calls .team_parse_fn helper.
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.league_resource_fn <- function(x) {

    league_list <- list(
        league_info = NULL,
        teams = NULL
        )

    league_list$league_info <-
        x %>%
        purrr::pluck(1) %>%
        dplyr::bind_rows()

    league_list$teams <-
        x %>%
        purrr::pluck(2, "teams") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map(purrr::pluck, "team") %>%
        purrr::map(.team_parse_fn) %>%
        purrr::map_df(purrr::map_if, purrr::negate(is.character), as.character)

    # df <- league_list$teams %>%
        # add_column(league_list$league_info, .before = 1)

    return(league_list)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             team resource parse                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' parse return from team resource
#'
#' helper function called by y_rosters.  this function calls .roster_parse_fn helper.
#'
#' @param x list passed on from .league_resource_fn
#'
#' @keywords internal
.team_parse_fn <- function(x) {
    team_list <- list(
        team = NULL,
        team_info = NULL,
        team_logo = NULL,
        roster_adds = NULL,
        managers = NULL,
        rosters = list(roster_info = NULL)
    )

    team_list$team <-
        x %>%
        purrr::pluck(1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact()

    team_list$team_info <-
        team_list$team %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::flatten() %>%
        purrr::keep(purrr::negate(purrr::is_list)) %>%
        dplyr::bind_rows() %>%
        dplyr::rename("team_name" = name, "team_url" = url)

    team_list$team_logo <-
        team_list$team %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::flatten() %>%
        purrr::pluck("team_logos") %>%
        purrr::flatten() %>%
        purrr::set_names(nm = paste(names(.), seq_along(.), sep = "_")) %>%
        unlist(recursive = T) %>%
        dplyr::bind_rows()

    team_list$roster_adds <-
        team_list$team %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::flatten() %>%
        purrr::pluck("roster_adds") %>%
        dplyr::bind_rows()

    team_list$managers <-
        team_list$team %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::flatten() %>%
        purrr::pluck("managers") %>%
        purrr::flatten() %>%
        purrr::set_names(nm = paste(names(.), seq_along(.), sep = "_")) %>%
        unlist(recursive = T) %>%
        dplyr::bind_rows()

    team_list$rosters$roster_info <- .roster_parse_fn(x)

    df <-
        purrr::reduce(team_list[2:length(team_list)], dplyr::bind_cols)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            roster resource parse                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' parse return from roster resource
#'
#' helper function called by y_rosters.  this function calls .player_parse_fn helper.
#'
#' @param x list passed on from .roster_resource_fn
#'
#' @keywords internal
.roster_parse_fn <- function(x) {
    roster_list <- list(
        roster = NULL,
        roster_info = NULL,
        minimum_games = NULL,
        player_info = NULL
    )

    roster_list$roster <-
        x %>%
        purrr::pluck(2, "roster")

    roster_list$roster_info <-
        roster_list$roster %>%
        purrr::keep(purrr::is_atomic) %>%
        dplyr::bind_cols()

    roster_list$minimum_games <-
        roster_list$roster %>%
        .["minimum_games"] %>%
        unlist() %>%
        dplyr::bind_rows() %>%
        dplyr::bind_cols()

    # roster_df <- reduce(roster_list[2:3], bind_cols) %>%
        # set_names(paste("roster", names(.), sep = "_"))

    # player_df <-
        # .player_parse_fn(roster_list[["roster"]][["0"]])

    roster_list$player_info$player_info = .player_parse_fn(roster_list[["roster"]][["0"]])

    # df <- bind_cols(roster_df, player_df)
    df <- purrr::reduce(roster_list[2:4], dplyr::bind_cols) %>%
        purrr::set_names(paste("roster", names(.), sep = "_"))


}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            player resource parse                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' parse return from player resource
#'
#' helper function called by y_rosters.  this function calls .player_parse_fn helper.
#'
#' @param x list passed on from .roster_resource_fn
#'
#' @keywords internal
.player_parse_fn <- function(x) {

    player_list <- list(
        player = NULL,
        player_info = NULL,
        player_selected_position = NULL
    )

    player_list$player <-
        x %>%
        purrr::pluck("players") %>%
        purrr::set_names(seq_along(.)) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact()

    player_list$player_info <-
        player_list$player %>%
        purrr::map(purrr::pluck, "player", 1) %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(purrr::compact) %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(purrr::compact) %>%
        purrr::map(purrr::flatten) %>%
        purrr::map(purrr::map_if, purrr::is_list, unlist) %>%
        purrr::map(purrr::map_at,
            "eligible_positions",
            purrr::set_names,
            nm = ~ paste(., seq_along(.), sep = "_")) %>%
        purrr::map(unlist) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows()

    player_list$player_selected_position <-
        player_list$player %>%
        purrr::map(purrr::pluck, "player", 2) %>%
        purrr::map(purrr::keep, purrr::is_list) %>%
        purrr::map(purrr::compact) %>%
        purrr::map(unlist) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows()

    df <- player_list[-1] %>% # drop 1st element with all the raw data in it
        compact() %>% # drop empty elements
        purrr::reduce(dplyr::bind_cols)
}

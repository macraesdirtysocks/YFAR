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
#'
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


#' team_id check
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
##                              PLAYER ID CHECK                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' player id check
#'
#' Checks for presence of supplied player id and validity
#'
#' @param player_id league_id supplied to y_function
#'
#' @keywords internal
.player_id_check <- function(player_id) {
    if (is.null(player_id) == TRUE) {
        stop(message("player_id argument required but not supplied"))
    } else if (is.character(player_id) == FALSE) {
        stop(message("player_id should be a character string"))
    } else if (stringr::str_detect(player_id, pattern = "[:digit:]*\\.p\\.[:digit:]*") == FALSE) {
        stop(message("player_id syntax should be 000.p.0000"))
    } else {
        invisible(player_id)
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
##                                URI_GEN_FUNC                              ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Generate uris.
#'
#' Arguments are passed on to `httr::modify_url()`.
#'
#' The Yahoo! Fantasy API will return a max of 25 players per call to the API.
#' This function divided by 25 and finds the remainder to construct the uri's
#' which are then passed to `y_get_response()`.
#'
#' @param league_id league_id as a string in the form "000.l.0000".  These ids can be found with `y_games()` and `y_teams()`.
#' @param resource resource from the api to be called
#' @param subresource subresource from the api to be called, takes a vector if more than one subresource
#' @param resp_len length of each response with a max of 25
#' @param start Return start number
#' @param number_of_players Count of players to return
#' @param ... api filter parameters
#'
#' @return a vector of strings
#' @keywords internal
.uri_gen_func <- function(resp_len = 25, league_id, resource, subresource, start, number_of_players, ...){

    # Params as list
    uri_params <- list(...)

    # Takes params list and makes element name the param and the element value the param value.
    # i.e. list(count = 5) gets converted to "count=5".
    uri_params <-
        if(!purrr::is_empty(uri_params)) {
            paste(names(uri_params), purrr::map_chr(uri_params, `[[`, 1), sep = "=", collapse = ";")
        } else{
            NULL
        }

    subresource <- ifelse(length(subresource) > 1, glue::glue_collapse(subresource, sep = "/"), subresource)

    resp_len <- ifelse(resp_len > 25, 25, resp_len)

    remainder <- number_of_players%%resp_len

    full_pages <- (number_of_players - remainder)

    full_pages_multiple <- full_pages/resp_len

    end <- full_pages + start

    page_start <- seq(from = start, to = end, by = resp_len)

    page_count <- c(rep(resp_len, times = full_pages_multiple), remainder)

    uri <- httr::modify_url(
        url = "https://fantasysports.yahooapis.com",
        path = paste("fantasy/v2", resource, league_id, subresource, sep = "/"),
        params = glue::glue(uri_params, "start={page_start}", "count={page_count}", .sep = ";", .null = NULL),
        query = "format=json"
    )

    uri <- uri[!grepl("count=0", uri)]

    return(uri)

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
#' @param uri URI being queried
#' @param token_name Oauth token value assign by y_create_token()
#'
#' @keywords internal
.y_get_response <- function(uri = NULL, token_name = NULL) {

    api_token = token_name

    r <- httr::RETRY(verb = "GET",
                     terminate_on = c(401),
                     url = uri,
                   httr::add_headers(
                       Authorization = stringr::str_c("Bearer",
                                                      api_token$credentials$access_token, sep = " ")
                   ))

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
#' check's date argument for format "%Y-%m-%d"
#'
#' @param date
#'
#' @keywords internal
.date_check <- function(x) {
    if(!is.na(as.Date(x, format = "%Y-%m-%d"))) {
        x
    } else {
        message("please supply a valid date in format %Y-%m-%d, returning rosters for today's date")
        Sys.Date()
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
##                                CURRENT WEEK                              ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Get current fantasy week
#'
#' @param game_id 3 digit number which prefixes league_id.
#' Identifies Yahoo Fanatasy Sport i.e. 411 is 2021  fantasy hockey.
#' Can be found with `y_games()`.
#'
#' @param token_name api token value assigned by `y_create_token()`
#'
#' @keywords internal
.current_week <- function(game_id = NULL, token_name = NULL){

    api_token <- token_name

    season_weeks <- YFAR::y_weeks(game_id, token_name = api_token)

    i <- purrr::map2(.x = season_weeks$start, .y = season_weeks$end, lubridate::interval)

    this_week <- season_weeks[purrr::map_lgl(.x = i, .f = ~lubridate::`%within%`(lubridate::now(), .x)),]

    return(this_week)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                Y GAMES PARSE                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Parse games resource
#'
#' @param x input list
#'
#' @return a tibble
#' @keywords internal
.y_games_parse <- function(x){

    game_meta <-
        x %>%
        purrr::pluck(1) %>%
        purrr::set_names(., nm = .col_name_change_fn(., "game")) %>%
        dplyr::bind_cols()

    game_leagues <-
        x %>%
        purrr::pluck(2, "leagues") %>%
        purrr::map(purrr::pluck, "league") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map_depth(2, ~ purrr::set_names(.x, nm = .col_name_change_fn(.x, "league"))) %>%
        purrr::map_df(unlist, recursive = TRUE)

    df <-
        dplyr::bind_cols(game_meta, game_leagues, .name_repair = janitor::make_clean_names)

    return(df)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                      CATEGORY LEAGUE SETTINGS FUNCTION                   ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#................CATEGORY LEAGUE SETTINGS FUNCTION...............


#' Parse category league settings
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


#' Parse points league settings
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


#' Parse team meta data
#'
#' helper function called by y_ functions to parse team meta data
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.team_meta_func <- function(x) {

    df <-
        x %>%
        purrr::pluck("team", 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::map_depth(1, unlist, recursive = TRUE) %>%
        purrr::map_depth(2, as.character) %>%
        purrr::map(~ purrr::set_names(.x, janitor::make_clean_names(names(.x)))) %>%
        purrr::flatten_df()

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


#' Parse stats data
#'
#' helper function called by y_matchups to parse team standings response
#'
#' @param x parsed content from response object
#'
#' @keywords internal
.stats_data_func <- function(x) {

    team_name <-
        x %>%
        purrr::pluck("team", 1) %>%
        magrittr::extract(1:3) %>%
        purrr::flatten_df()

    stats <-
        x %>%
        purrr::pluck("team", 2) %>%
        purrr::map_at("team_stats", purrr::map_at, "stats", purrr::map_depth, 2, purrr::map_at, "value", as.double) %>%
        purrr::map_at("team_remaining_games", purrr::map_at, "total", purrr::map_depth, 2, as.double) %>%
        purrr::map_at("team_points", purrr::map_at, c("week","total"), as.double) %>%
        purrr::flatten() %>%
        purrr::map_if(purrr::is_list, purrr::flatten_df) %>%
        purrr::map_at(
            "stats",
            ~ dplyr::left_join(., .yahoo_hockey_stat_categories(), by = "stat_id") %>%
                dplyr::select(display_name, value) %>%
                tidyr::pivot_wider(
                    names_from = display_name,
                    values_from = value,
                    names_prefix = "count_")
        ) %>%
        dplyr::bind_cols(.name_repair = janitor::make_clean_names) %>%
        dplyr::select(!tidyselect::matches("_[[:digit:]]$"))


    df <- dplyr::bind_cols(team_name, stats)

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

#' Parse return from league resource
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
        purrr::pluck("league", 1) %>%
        dplyr::bind_rows()

    league_list$teams <-
        x %>%
        purrr::pluck("league", 2, "teams") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map_df(.team_parse_fn)


    return(league_list)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             team resource parse                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Parse return from team resource
#'
#' helper function called by y_rosters.  this function calls .roster_parse_fn helper.
#'
#' @param x list passed on from .league_resource_fn
#'
#' @keywords internal
.team_parse_fn <- function(x) {

    team_meta <-
        x %>%
        purrr::pluck("team", 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::map_depth(1, unlist, recursive = TRUE) %>%
        purrr::map_depth(2, as.character) %>%
        purrr::map(~purrr::set_names(.x, janitor::make_clean_names(names(.x))))

    roster <- .roster_parse_fn(x, "team", 2, "roster")

    df <- dplyr::bind_cols(team_meta, roster)

    return(df)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            roster resource parse                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Parse return from roster resource
#'
#' helper function called by y_rosters.  this function calls .player_parse_fn helper.
#'
#' @param x list passed on from .roster_resource_fn
#' @param ... arguments passed on to purrr::pluck
#'
#' @keywords internal
.roster_parse_fn <- function(x, ...) {

        preprocess <-
            x %>%
            purrr::pluck(...)

        roster_meta <-
            preprocess %>%
            purrr::keep(purrr::is_bare_atomic) %>%
            dplyr::bind_cols()

        player_data <-
            preprocess %>%
            purrr::pluck("0", "players") %>%
            purrr::keep(purrr::is_list) %>%
            purrr::flatten() %>%
            purrr::map_df(.player_parse_fn)

        goalie_min_games <-
            preprocess %>%
            purrr::pluck("minimum_games") %>%
            dplyr::bind_cols()

        df <- dplyr::bind_cols(
            roster_meta,
            player_data,
            goalie_min_games,
            .name_repair = janitor::make_clean_names)

        return(df)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            player resource parse                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Parse return from player resource
#'
#' helper function called by y_rosters.  This function calls .player_parse_fn helper.
#'
#' @param x list passed on from .roster_resource_fn
#'
#' @keywords internal
.player_parse_fn <- function(x) {

    df <-
        x %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::map(unlist, recursive = TRUE) %>%
        purrr::map(~ purrr::set_names(.x, janitor::make_clean_names(names(.x)))) %>%
        purrr::flatten_df()

        return(df)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                             PLAYER STATS PARSE                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Parse player stats
#'
#' @param x element to parse
#'
#' @return a tibble
#' @keywords internal
.player_stats_parse <- function(x){

basic_stats <-
    x %>%
    purrr::pluck(2, "player_stats", "stats") %>%
    purrr::map_df(purrr::flatten_df)

advanced_stats <-
    x %>%
    purrr::pluck(2, "player_advanced_stats", "stats") %>%
    purrr::map_df(purrr::flatten_df)


stats <-
    dplyr::bind_rows(basic_stats, advanced_stats) %>%
    dplyr::left_join(., .yahoo_hockey_stat_categories(), by = "stat_id") %>%
    dplyr::select(display_name, value) %>%
    tidyr::pivot_wider(id_cols = display_name,
                       names_from = display_name,
                       values_from = value)

return(stats)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                         TRANSACTION PARSE FUNCTION                       ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Parse transactions resource
#'
#' This function call .player_parse_fn
#'
#' @param x element to parse
#'
#' @return a tibble
#' @keywords internal
.transaction_parse_fn <- function(x) {

    transaction_meta <-
        x %>%
        purrr::pluck(1) %>%
        dplyr::bind_cols()

    players <-
        x %>%
        purrr::pluck(2, "players") %>%
        purrr::map(purrr::pluck, "player") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::map_df(.player_parse_fn)

    df <-
        dplyr::bind_cols(transaction_meta, players)

    return(df)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                MATCHUP PARSE                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Parse matchup subresource
#'
#' Called by y_matchups
#'
#' @param x input
#'
#' @return a tibble or list
#' @keywords internal
.matchup_parse_fn <- function(x) {

    matchup_meta <-
        x %>%
        purrr::keep(purrr::is_atomic) %>%
        purrr::map(as.character) %>%
        dplyr::bind_cols()

    stat_winners <-
        x %>%
        purrr::pluck("stat_winners") %>%
        purrr::map_depth(3, as.character) %>%
        purrr::flatten_df() %>%
        ##convert stat id numbers to display name i.e. stat 1 = G
        dplyr::left_join(.yahoo_hockey_stat_categories(), by = "stat_id") %>%
        dplyr::select(display_name, 2) %>%
        tidyr::pivot_wider(names_from = display_name,
                           values_from = 2)

    matchup_team_data <-
        x %>%
        purrr::pluck("0", "teams") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map(.stats_data_func) %>%
        dplyr::bind_rows()

    df <-
        dplyr::bind_cols(matchup_meta, matchup_team_data, stat_winners, .name_repair = janitor::make_clean_names) %>%
        dplyr::select(!tidyselect::matches("_[[:digit:]]$"))

    return(df)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        YAHOO HOCKEY STAT CATEGORIES                      ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Create a tibble of the stat categories for yahoo fantasy hockey
#'
#' @return a tibble
#' @keywords internal
.yahoo_hockey_stat_categories <- function(){
    structure(
        list(
            stat_id = c(
                "0",
                "1",
                "2",
                "3",
                "4",
                "5",
                "6",
                "7",
                "8",
                "9",
                "10",
                "11",
                "12",
                "13",
                "14",
                "15",
                "16",
                "17",
                "18",
                "19",
                "20",
                "21",
                "22",
                "23",
                "24",
                "25",
                "26",
                "27",
                "28",
                "29",
                "30",
                "31",
                "32",
                "33",
                "34",
                "1001",
                "1002",
                "1003",
                "1004",
                "1005",
                "1006",
                "1007",
                "1008",
                "1009",
                "1010",
                "1011"
            ),
            name = c(
                "Games Played",
                "Goals",
                "Assists",
                "Points",
                "Plus/Minus",
                "Penalty Minutes",
                "Powerplay Goals",
                "Powerplay Assists",
                "Powerplay Points",
                "Shorthanded Goals",
                "Shorthanded Assists",
                "Shorthanded Points",
                "Game-Winning Goals",
                "Game-Tying Goals",
                "Shots on Goal",
                "Shooting Percentage",
                "Faceoffs Won",
                "Faceoffs Lost",
                "Games Started",
                "Wins",
                "Losses",
                "Ties",
                "Goals Against",
                "Goals Against Average",
                "Shots Against",
                "Saves",
                "Save Percentage",
                "Shutouts",
                "Time on Ice",
                "F/D Games",
                "Goalie Games",
                "Hits",
                "Blocks",
                "Time on Ice",
                "Average Time on Ice",
                "Power Play Time",
                "Average Power Play Time",
                "Short-Handed Time",
                "Average Short-Handed Time",
                "Corsi",
                "Fenwick",
                "Offensive Zone Starts",
                "Defensive Zone Starts",
                "Zone Start Percentage",
                "Game Star",
                "Shifts"
            ),
            display_name = c(
                "gp",
                "g",
                "a",
                "p",
                "x",
                "pim",
                "ppg",
                "ppa",
                "ppp",
                "shg",
                "sha",
                "shp",
                "gwg",
                "gtg",
                "sog",
                "sh_percent",
                "fw",
                "fl",
                "gs",
                "w",
                "l",
                "t",
                "ga",
                "gaa",
                "sa",
                "sv",
                "sv_percent",
                "sho",
                "toi",
                "gp_2",
                "gp_3",
                "hit",
                "blk",
                "toi_2",
                "toi_g",
                "ppt",
                "avg_ppt",
                "sht",
                "avg_sht",
                "cor",
                "fen",
                "off_zs",
                "def_zs",
                "zs_pct",
                "g_str",
                "shifts"
            ),
            sort_order = c(
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "0",
                "1",
                "1",
                "0",
                "1",
                "0",
                "0",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1",
                "1"
            ),
            is_composite_stat = c(
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                1L,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                1L,
                NA,
                NA,
                1L,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                1L,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA
            )
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA, -46L)
    )}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            .COL_NAME_CHANGE_FN                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Prefix column names
#'
#' Used internally for prefixing resource and subresource names to column names.
#'
#' @param x String to detect in column names. Passed to `names()`.
#' @param y Prefix string
#'
#' @return a sting
#' @keywords internal
.col_name_change_fn <- function(x, y){

    ifelse(grepl(y, names(x)), names(x), paste(y, names(x), sep = "_"))
}


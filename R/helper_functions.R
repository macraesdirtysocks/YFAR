##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            TOKEN CHECK FUNCTIONS                         ----
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
##                             RESPONSE FUNCTIONS                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#....................Y_GET_RESPONSE FUNCTION.....................


#' y_get_response
#'
#' Send GET request to YAHOO! api
#'
#' @param uri URI being queried.
#' @param token_name Oauth token value assign by `y_create_token()`.
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
##                            META PARSE FUNCTIONS                          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#........................GAME META PARSE.........................


#' Parse meta from game resource
#'
#
#' @param x Game resource
#'
#' @keywords internal
.game_meta_parse_fn <- function(x) {

game_meta <-
    x %>%
    purrr::pluck("game", 1) %>%
    purrr::map(as.character) %>%
    dplyr::bind_cols() %>%
    dplyr::rename_with(~ paste("game", .x, sep = "_"), .cols = !tidyselect::matches("^game_"))
}

#........................LEAGUE MEAT PARSE.......................

#' Parse meta from league resource
#'
#' @param x League resource
#'
#' @keywords internal
.league_meta_parse_fn <- function(x) {

    league_info <-
        x %>%
        purrr::pluck("league", 1) %>%
        purrr::map(as.character) %>%
        dplyr::bind_rows() %>%
        dplyr::rename_with(~ paste("league", .x, sep = "_"), .cols = !tidyselect::matches("^league_"))

    return(league_info)
}


#........................TEAM MEAT PARSE.........................

#' Parse team meta data element
#'
#' @param x Team resource
#'
#' @keywords internal
.team_meta_parse_fn <- function(x) {

    df <-
        x %>%
        purrr::pluck("team", 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::flatten() %>%
        purrr::map_if(purrr::is_list, ~unlist(.x, recursive = TRUE) %>% tibble::as_tibble_row(.name_repair = janitor::make_clean_names)) %>%
        dplyr::bind_cols() %>%
        dplyr::mutate(dplyr::across(.cols = tidyselect::everything(), .fns = as.character)) %>%
        dplyr::rename_with(~ paste("team", .x, sep = "_"), .cols = !tidyselect::matches("^team_")) %>%
        janitor::clean_names()

    return(df)
}


#........................PLAYER META PARSE.......................


#' Parse meta data element from player resource.
#'
#' @param x Player resource
#'
#' @keywords internal
.player_meta_parse_fn <- function(x) {

    df <-
        x %>%
        purrr::pluck("player", 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::flatten() %>%
        purrr::map_if(purrr::is_list, ~unlist(.x) %>% tibble::as_tibble_row(.name_repair = janitor::make_clean_names)) %>%
        dplyr::bind_cols(.name_repair = janitor::make_clean_names) %>%
        dplyr::rename_with(~ paste("player", .x, sep = "_"), .cols = !tidyselect::matches("^player_"))

    return(df)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          RESOURCE PARSE FUNCTIONS                        ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# These helper functions are called in succession depending on what level
# of the Yahoo Fantasy API is queried.

# League -> teams -> roster -> players

# A league is made up of teams, teams are made up of rosters and rosters
# are made up of players.

# Essentially what I attempted to do is standardize where each functions starts
# so when the functions index into the and hit a particular element that element
# is then fed to the next parsing function.


#......................GAMES RESOURCE PARSE......................

#' Parse game resource
#'
#' This function parses the games resource.
#'
#' The ... argument allows passing an addition parse function to parse addition
#' sub-resources.
#'
#' @param x Games resource.
#' @param ... Additional parse function passed to purrr::map_df.
#'
#' @keywords internal
.game_resource_parse_fn <- function(x, ...) {

    game_meta <-
        x %>%
        .game_meta_parse_fn()

    subresource <-
        x %>%
        purrr::pluck("game", 2, 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map_df(...)

    df <- dplyr::bind_cols(game_meta, subresource, .name_repair = janitor::make_clean_names) %>%
        dplyr::select(!tidyselect::matches("_[[:digit:]]$"))

    return(df)
}


#......................LEAGUE RESOURCE PARSE.....................


#' Parse return from league resource
#'
#' This function parses the league resource.
#'
#' The ... argument allows passing an addition parse function to parse addition
#' sub-resources.
#'
#' Sub-resource attached to team could be draft_results, roster, team_stats,
#' standings or match-ups.
#'
#' @param x Leagues resource.
#' @param ... Additional parse function passed to purrr::map_df.
#'
#' @keywords internal
.league_resource_parse_fn <- function(x, ...) {

    league_info <-
        x %>%
        .league_meta_parse_fn()

    subresource <-
        x %>%
        purrr::pluck("league", 2, 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map_df(...)

    df <- dplyr::bind_cols(league_info, subresource, .name_repair = janitor::make_clean_names) %>%
        dplyr::select(!tidyselect::matches("_[[:digit:]]$"))

    return(df)
}


#......................TEAM RESOURCE PARSE.......................


#' Parse return from team resource
#'
#' This function parses the teams resource as well as the attached teams sub-resource.
#'
#' The ... argument allows passing an addition parse function to parse addition
#' sub-resources.
#'
#' @param x Team resource.
#' @param ... Additional parse function passed to purrr::map_df.
#'
#' @keywords internal
.team_resource_parse_fn <- function(x, ...) {

    team_meta <-
        x %>%
        .team_meta_parse_fn()

    subresource <-
        x %>%
        purrr::pluck("team", 2, 1)

    # Here reacts to a special case where the team sub-resource is roster.
    # Because one teams can only have one roster mapping fails so we need to use do.call.
    # If every element of the sub-resource is a list function will map else it will use do.call.
    # Whne roster is plucked out via purrr::pluck("team", 2, 1) we have the roster sub-resource data on hand
    # so we don't need to map into an element to get it.
    if("count" %in% names(subresource)){
        subresource <-
            subresource %>%
            purrr::keep(purrr::is_list) %>%
            purrr::map_df(...)
    } else {
        subresource <- do.call(..., list(subresource))
    }

    df <- dplyr::bind_cols(team_meta,
                           subresource,
                           .name_repair = janitor::make_clean_names) %>%
        dplyr::select(!tidyselect::matches("_[[:digit:]]$"))

    return(df)

}


#......................ROSTER RESOURCE PARSE.....................


#' Parse roster resource.
#'
#' This function parses the rosters resource.
#'
#' Calls .player_resource_parse_fn because rosters have players.
#'
#' @param x Roster resource.
#'
#' @keywords internal
.roster_resource_parse_fn <- function(x) {

    roster_meta <-
        x %>%
        purrr::keep(purrr::is_bare_atomic) %>%
        dplyr::bind_cols() %>%
        dplyr::rename_with(~ paste("roster", .x, sep = "_"), .cols = !tidyselect::matches("^roster_"))

    player_data <-
        x %>%
        purrr::pluck("0", "players") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::map_df(.player_resource_parse_fn, .two_deep_subresource_parse)

    other_elements <-
        x %>%
        purrr::keep(purrr::is_list) %>%
        purrr::discard(names(.) == "0") %>%
        purrr::map_df(.two_deep_subresource_parse)


    df <- dplyr::bind_cols(
        roster_meta,
        player_data,
        other_elements,
        .name_repair = janitor::make_clean_names) %>%
        dplyr::select(!tidyselect::matches("_[[:digit:]]$"))


    return(df)

}


#......................PLAYER RESOURCE PARSE.....................


#' Parse player resource.
#'
#' @param x Player resource.
#' @param ... Additional parse function passed to purrr::map_df.
#'
#' @keywords internal
.player_resource_parse_fn <- function(x, ...) {

    player_meta <-
        x %>%
        .player_meta_parse_fn()

    # x[1] is the player_meta which is parsed above.  This takes everything else.
    subresource <-
        x %>%
        purrr::pluck("player") %>%
        magrittr::extract(-1) %>%
        purrr::map_dfc(...)

    df <-
        dplyr::bind_cols(player_meta, subresource)

    return(df)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        SUB-RESOURCE PARSE FUNCTIONS                      ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#......................LEAGUE SETTINGS PARSE.....................

#' Parse league settings.
#'
#' @param x Settings sub-resource.
#'
#' @keywords internal
.league_settings_parse_fn <- function(x){

    # Initialize empty list.
    settings_list <- list()
    # Get names of elements in settings list.
    settings_names <- names(purrr::pluck(x, 1))
    # League scoring type.
    league_scoring <- purrr::pluck(x, 1, "scoring_type")
    # %>% magrittr::extract("scoring_type")

    # General league settings.  These are already atomic and can be plucked with keep.
    settings_list$league_settings =
        x %>%
        purrr::pluck(1) %>%
        purrr::keep(purrr::negate(purrr::is_list)) %>%
        dplyr::bind_rows() %>%
        tidyr::nest(league_settings = tidyselect::everything())

    # Roster positions list element.
    settings_list$roster_positions =
        x %>%
        purrr::pluck(1, "roster_positions") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(~dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = tidyselect::everything(), as.character))) %>%
        tidyr::nest(roster_positions = tidyselect::everything())

    # Stat categories list element.
    settings_list$stat_categories =
        x %>%
        purrr::pluck(1, "stat_categories", "stats") %>%
        purrr::map(purrr::pluck, 1) %>%
        purrr::transpose() %>%
        purrr::map_at("stat_position_types", purrr::map, purrr::pluck, 1, "stat_position_type", "position_type") %>%
        purrr::transpose() %>%
        purrr::map_df(dplyr::bind_rows) %>%
        tidyr::nest(stat_categories = tidyselect::everything())

    # If league has divisions.
    if("divisions" %in% settings_names) {
        settings_list$divisions =
            x %>%
            purrr::pluck(1, "divisions") %>%
            purrr::flatten_dfr() %>%
            tidyr::nest(divisions = tidyselect::everything())
    }

    # If league has fixed waiver days.
    if("waiver_days" %in% settings_names) {
        settings_list$waiver_days =
            x %>%
            purrr::pluck(1, "waiver_days") %>%
            dplyr::bind_rows() %>%
            tidyr::nest(waiver_days = tidyselect::everything())
    }

    # If league uses points.
    if(league_scoring == "point") {
        settings_list$stat_modifiers =
            x %>%
            purrr::pluck(1, "stat_modifiers", "stats") %>%
            purrr::map_df(purrr::pluck, 1) %>%
            tidyr::nest(stat_modifiers = tidyselect::everything())
    }

    df <-
        purrr::reduce(settings_list, dplyr::bind_cols)

    return(df)
}

#........................STANDINGS PARSE.........................


#' Parse standings sub-resource
#'
#' @param x List element containing standings resource.
#'
#' @keywords internal
.standings_parse_fn <- function(x) {

    team_meta <-
        x %>%
        .team_meta_parse_fn()

    team_stats <-
        x %>%
        purrr::pluck("team", 2, "team_stats", "stats") %>%
        purrr::flatten_df() %>%
        dplyr::left_join(., .yahoo_hockey_stat_categories(), by = "stat_id") %>%
        dplyr::select("display_name", "value") %>%
        tidyr::pivot_wider(
            id_cols = display_name,
            names_from = display_name,
            values_from = value)

    team_points <-
        x %>%
        purrr::pluck("team", 2, "team_points") %>%
        purrr::flatten_df()

    standings <-
        x %>%
        purrr::pluck("team", 3, "team_standings") %>%
        unlist(recursive = TRUE) %>%
        dplyr::bind_rows() %>%
        janitor::clean_names()

    df <- dplyr::bind_cols(team_meta, team_points, standings, team_stats)

    return(df)

}


#........................TRANSACTION PARSE.......................


#' Parse transactions resource.
#'
#' This function call .player_parse_fn
#'
#' @param x element to parse
#' @param ... element to parse
#'
#' @return a tibble
#' @keywords internal
.transaction_parse_fn <- function(x, ...) {

    transaction_meta <-
        x %>%
        purrr::pluck("transaction", 1) %>%
        dplyr::bind_cols() %>%
        dplyr::rename_with(
            ~ paste("transaction", .x, sep = "_"), .cols = !tidyselect::matches("^transaction_"))

    subresource <-
        x %>%
        purrr::pluck("transaction", 2, 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map_df(...)

    df <-
        dplyr::bind_cols(transaction_meta, subresource)

    return(df)
}


#..........................MATCHUP PARSE.........................


#' Parse match-up sub-resource
#'
#' This function parses the match-up sub-resource.
#'
#' Used to parsed a list with a "match-up" element.
#'
#' Right now as a default it calls .yahoo_hockey_stat_categories() which converts stat_id numbers into
#' more readable word abbreviation categories i.e. converts 1 into g.
#'
#' @param x List containing a matchup element.
#'
#' @return A tibble
#' @keywords internal
.matchup_parse_fn <- function(x) {

    matchup_meta <-
        x %>%
        purrr::pluck("matchup") %>%
        purrr::keep(purrr::is_atomic) %>%
        purrr::map(as.character) %>%
        dplyr::bind_cols()

    stat_winners <-
        x %>%
        purrr::pluck("matchup", "stat_winners") %>%
        purrr::map_depth(3, as.character) %>%
        purrr::flatten_df() %>%
        ##convert stat id numbers to display name i.e. stat 1 = G
        dplyr::left_join(.yahoo_hockey_stat_categories(), by = "stat_id") %>%
        dplyr::select(display_name, 2) %>%
        tidyr::pivot_wider(names_from = display_name,
                           values_from = 2) %>%
        dplyr::rename_with(.fn = ~paste(.x, "winner", sep = "_"), .cols = tidyselect::everything())

    matchup_team_meta <-
        x %>%
        purrr::pluck("matchup", "0", "teams") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map_df(.team_meta_parse_fn)

    matchup_team_stats <-
        x %>%
        purrr::pluck("matchup", "0", "teams") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map_df(.team_stats_parse_fn)

    df <-
        dplyr::bind_cols(
            matchup_meta,
            matchup_team_meta,
            matchup_team_stats,
            stat_winners,
            .name_repair = janitor::make_clean_names
        ) %>% dplyr::mutate(
            dplyr::across(.cols = tidyselect::everything(), as.character))

    return(df)

}


#...................SETTINGS SUBRESOURCE PARSE...................

#' Parse settings sub-resource
#'
#' This function parses the settings sub-resource.
#'
#' Called by `y_league_settings()`.
#'
#' @param x List containing a matchup element.
#' @param ... Takes a function and is passed on to do.call.
#'
#' @return A tibble
#' @keywords internal
.settings_subresource_parse_fn <- function(x, ...) {

    sport_type <- purrr::pluck(x, "league", 1, "game_code")

    league_info <-
        x %>%
        .league_meta_parse_fn() %>%
        tidyr::nest(league_meta = !c(league_key))

    subresource <-
        do.call(..., list(purrr::pluck(x, "league", 2, 1)))

    df <-
        dplyr::bind_cols("sport" = sport_type,
                         league_info,
                         subresource,
                         .name_repair = janitor::make_clean_names) %>%
        dplyr::select(!tidyselect::matches("_[[:digit:]]$"))

    return(df)

}


#......................DRAFT ANALYSIS PARSE......................


#' Parse draft analysis sub-resource
#'
#' @param x List containing a draft analysis element.
#' @keywords internal
.draft_analysis_func <- function(x) {

    # Parse player meta data with internal function.
    player_meta <-
        .player_meta_parse_fn(x)

    # Parse draft analysis.
    draft_analysis <-
        x %>%
        purrr::pluck("player", 2, "draft_analysis") %>%
        dplyr::bind_cols()

    # Bind cols
    df <-
        dplyr::bind_cols(player_meta, draft_analysis)

    # Return df
    return(df)

}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ STATS PARSE FUNCTIONS  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#..................GAME STAT CATEGORIES PARSE FN.................


#' Parse game stats data.
#'
#' @param x Stats sub-resource of games resource
#'
#' @keywords internal
.game_stats_category_parse_fn <- function(x){


    game_meta <-
        x %>%
        purrr::pluck(1) %>%
        .game_meta_parse_fn()

    stats <-
        x %>%
        purrr::pluck(1, "game", 2, "stat_categories", "stats") %>%
        purrr::map(rlang::squash) %>%
        purrr::map(purrr::set_names, nm = janitor::make_clean_names) %>%
        purrr::map_depth(2, as.character) %>%
        dplyr::bind_rows() %>%
        # Some stats such as those expressed as percentages include the stats that are used in their calculation in a list called "stat_base".
        dplyr::rename_with(
            .fn = ~stringr::str_replace(.x, pattern = "(stat_id)(_[:digit:])", replacement = "base_\\1") %>% janitor::make_clean_names(),
            .cols = tidyselect::matches("stat_id_[[:digit:]]")
        ) %>%
        dplyr::bind_cols(game_meta, .) %>%
        tidyr::nest(stats = !game_key)


    advanced_stats <-
        x %>%
        purrr::pluck(2, "game", 2, "advanced_stat_categories", "stats") %>%
        purrr::map(rlang::squash) %>%
        purrr::map(purrr::set_names, nm = janitor::make_clean_names) %>%
        purrr::map_depth(2, as.character) %>%
        dplyr::bind_rows() %>%
        dplyr::bind_cols(game_meta, .) %>%
        tidyr::nest(advanced_stats = !game_key)


    df <- dplyr::full_join(stats, advanced_stats, by = "game_key")

    return(df)

}

#......................TEAM STATS PARSE FN.......................


#' Parse team stats data.
#'
#' @param x Stats sub-resource of teams resource
#'
#' @keywords internal
.team_stats_parse_fn <- function(x) {

    stats <-
        x %>%
        purrr::pluck("team", 2, "team_stats", "stats") %>%
        purrr::map(1) %>%
        purrr::map_depth(2, as.character) %>% #added this as a hunch, remove if trouble.
        purrr::map_df(dplyr::bind_cols) %>%
        dplyr::left_join(., .yahoo_hockey_stat_categories(), by = "stat_id") %>%
        dplyr::select(display_name, value) %>%
        tidyr::pivot_wider(names_from = display_name,
                           values_from = value,
                           names_prefix = "count_")

    other_elements <-
        x %>%
        purrr::pluck("team", 2) %>%
        purrr::discard(names(.) == "team_stats") %>%
        purrr::map_if(purrr::is_list, ~unlist(.x) %>% dplyr::bind_rows()) %>%
        dplyr::bind_cols(.name_repair = janitor::make_clean_names) %>%
        dplyr::select(!tidyselect::matches("_[[:digit:]]$"))

    df <- dplyr::bind_cols(stats, other_elements)

    return(df)

}


#......................PLAYER STATS PARSE FN.....................


#' Parse player stats
#'
#' Parse the stats resource of a player collection
#'
#' @param x element to parse
#'
#' @return a tibble
#' @keywords internal
.player_stats_parse <- function(x){

    coverage <-
        x %>%
        purrr::pluck("player_stats", "0") %>%
        dplyr::bind_cols()

    # basic_stats <-
    #     x %>%
    #     purrr::pluck("player_stats", "stats") %>%
    #     purrr::map_df(purrr::flatten_df)
    #     dplyr::left_join(., .yahoo_hockey_stat_categories(), by = "stat_id") %>%
    #     dplyr::select(display_name, value) %>%
    #     tidyr::pivot_wider(id_cols = display_name,
    #                        names_from = display_name,
    #                        values_from = value)
    #
    # advanced_stats <-
    #     x %>%
    #     purrr::pluck("player_advanced_stats", "stats") %>%
    #     purrr::map_df(purrr::flatten_df) %>%
    #     dplyr::left_join(., .yahoo_hockey_stat_categories(), by = "stat_id") %>%
    #     dplyr::select(display_name, value) %>%
    #     tidyr::pivot_wider(id_cols = display_name,
    #                        names_from = display_name,
    #                        values_from = value)

    player_stats <-
        x %>%
        purrr::pluck("player_stats", "stats") %>%
        purrr::flatten() %>%
        purrr::keep(grepl("stat", names(.))) %>%
        purrr::map_df(purrr::flatten_df) %>%
        dplyr::left_join(., .yahoo_hockey_stat_categories(), by = "stat_id") %>%
        dplyr::select(display_name, value) %>%
        tidyr::pivot_wider(id_cols = display_name,
                           names_from = display_name,
                           values_from = value)

    stats <-
        dplyr::bind_cols(coverage, player_stats)

    return(stats)

}





#...................TWO DEEP SUBRESOURCE PARSE...................


#' Parse a subresource that list a simple list of 1 deep lists.
#'
#' Function takes the name of the list and prefixes it any columns that
#' don't start with that prefix.
#'
#' @param x Subresource containing lists of depth 1.
#'
#' @return A tibble
#'
#' @keywords internal
.two_deep_subresource_parse <- function(x){

    y <- names(x)

    x %>%
        unlist(recursive = TRUE) %>%
        dplyr::bind_rows() %>%
        janitor::clean_names() %>%
        dplyr::rename_with(~ paste(y, .x, sep = "_"), .cols = !tidyselect::matches(glue::glue("^{y}_")))

}




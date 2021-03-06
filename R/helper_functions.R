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

#' Preprocess list
#'
#' Convert elements to character using `as.character()`
#' Prune unwanted `count` eleements.
#'
#' @param x List to preprocess
#'
#' @return A list
#' @keywords internal
list_pre_process_fn <- function(x){

  x %>%
    rrapply::rrapply(
      f = function(x) as.character(x),
      how = "recurse"
    ) %>%
    rrapply::rrapply(
      condition = function(x, .xname) .xname != "count",
      how = "prune"
    )

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
# so when the functions index into the list and hit a particular element that element
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
#' @param pluck_args List of arguments passed to purrr::pluck.
#' @param fn Function to run on sub-resource.
#'
#' @return A tibble.
#'
#' @keywords internal
.game_resource_parse_fn <- function(x, pluck_args = NULL, fn) {

  data_list <- list(
    league_meta = NULL,
    subresource_df = NULL
  )

  data_list$game_meta <-
    x %>%
    purrr::pluck("game", 1) %>%
    purrr::lmap(.unlist_and_bind_fn) %>%
    dplyr::bind_cols() %>%
    dplyr::rename_with(
      ~ paste("game", .x, sep = "_"),
      .cols = !tidyselect::matches("^game_")
    )

  if (!is.null(pluck_args)) {

    subresource_element <-
      purrr::pluck(x, !!!pluck_args)

    fn_todo <-
      rlang::expr(fn(subresource_element))

    data_list$subresource_df <-
      rlang::eval_tidy(fn_todo)
  }

  df <-
    data_list %>%
    purrr::compact() %>%
    purrr::reduce(dplyr::bind_cols)

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
#' @param pluck_args List of arguments passed to purrr::pluck.
#' @param fn Function to run on subresource.
#'
#' @return A tibble.
#'
#' @keywords internal
.league_resource_parse_fn <- function(x, pluck_args = NULL, fn) {

  data_list <- list(
    league_meta = NULL,
    subresource_df = NULL
  )

    data_list$league_meta <-
      x %>%
      purrr::pluck("league", 1) %>%
      purrr::lmap(.unlist_and_bind_fn) %>%
      dplyr::bind_cols() %>%
      dplyr::rename_with(
        ~ paste("league", .x, sep = "_"), .cols = !tidyselect::matches("^league_"))

    if(!is.null(pluck_args)){

      subresource_element <-
        purrr::pluck(x, !!!pluck_args)

      fn_todo <-
        rlang::expr(fn(subresource_element))

      data_list$subresource_df <-
        rlang::eval_tidy(fn_todo)
    }

    df <-
      data_list %>%
      purrr::compact() %>%
      purrr::reduce(dplyr::bind_cols)

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
#' @param pluck_args List of arguments passed to purrr::pluck.
#' @param fn Function to run on subresource.
#'
#' @return A tibble.
#'
#' @keywords internal
.team_resource_parse_fn <- function(x, pluck_args = NULL, fn) {

  data_list <- list(
    team_meta = NULL,
    subresource_df = NULL
  )

  data_list$team_meta <-
    x %>%
    purrr::pluck("team", 1) %>%
    purrr::lmap(.unlist_and_bind_fn) %>%
    dplyr::bind_cols() %>%
    dplyr::rename_with(
      ~ paste("team", .x, sep = "_"), .cols = !tidyselect::matches("^team_"))


  if(!is.null(pluck_args)){

  subresource_element <-
    purrr::pluck(x, !!!pluck_args)

  fn_todo <-
    rlang::expr(fn(subresource_element))

  data_list$subresource_df <-
    rlang::eval_tidy(fn_todo)
  }

  df <-
    data_list %>%
    purrr::compact() %>%
    purrr::reduce(dplyr::bind_cols)

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
#' @param pluck_args List of arguments passed to purrr::pluck.
#' @param fn Function to run on sub-resource.
#'
#' @return A tibble.
#'
#' @keywords internal
.roster_resource_parse_fn <- function(x, pluck_args, fn) {

  df <-
  x %>%
    purrr::flatten() %>%
    purrr::set_names(nm = ~ paste("roster", .x, sep = "_")) %>%
    purrr::modify_at("roster_0",
                     purrr::compose(
                       .dir = "forward",
                       ~ purrr::pluck(.x, "players"),
                       ~ purrr::map_df(
                         .x,
                         .player_resource_parse_fn,
                         pluck_args = list("player", 2),
                         fn = function(x)
                           purrr::lmap(x, .unlist_and_bind_fn) %>% dplyr::bind_rows()
                       )
                     )) %>%
    purrr::modify_at("roster_minimum_games",
                     purrr::compose(~ purrr::set_names(
                       .x, nm = ~ paste("minimum_games", .x, sep = "_")
                     ),
                     ~ dplyr::bind_cols(.x))) %>%
    purrr::modify_at(
      "roster_outs_pitched",
      purrr::compose(
        .dir = "forward",
        ~ purrr::set_names(.x, nm = ~ paste("outs_pitched", .x, sep = "_")),
        ~ dplyr::bind_cols(.x)
      )

    ) %>%
    dplyr::bind_cols()
  # roster_meta <-
  #   x %>%
  #   purrr::keep(purrr::is_bare_atomic) %>%
  #   dplyr::bind_cols() %>%
  #   dplyr::rename_with( ~ paste("roster", .x, sep = "_"),
  #                       .cols = !tidyselect::matches("^roster_"))
  #
  # player_data <-
  #   x %>%
  #   purrr::pluck("0", "players") %>%
  #   # purrr::keep(purrr::is_list) %>%
  #   purrr::compact() %>%
  #   purrr::map_df(.player_resource_parse_fn,
  #                 pluck_args = list("player", 2),
  #                 fn = function(x) purrr::lmap(x, ~.unlist_and_bind_fn(.x) %>% dplyr::bind_cols()))
  #
  # other_elements <-
  #   x %>%
  #   purrr::keep(purrr::is_list) %>%
  #   purrr::discard(names(.) == "0") %>%
  #   purrr::lmap(.unlist_and_bind_fn) %>%
  #   dplyr::bind_cols()
  #
  # df <-
  #   dplyr::bind_cols(purrr::compact(list(
  #     roster_meta, player_data, other_elements)),
  #   .name_repair = janitor::make_clean_names) %>%
  #   dplyr::select(!tidyselect::matches("_[[:digit:]]$"))


  return(df)

}


#......................PLAYER RESOURCE PARSE.....................


#' Parse player resource.
#'
#' @param x Player resource.
#' @param pluck_args List of arguments passed to purrr::pluck.
#' @param fn Function to run on subresource.
#'
#' @return A tibble.
#'
#' @keywords internal
.player_resource_parse_fn <- function(x, pluck_args = NULL, fn) {

  data_list <- list(
    player_meta = NULL,
    subresource_df = NULL
  )

  data_list$player_meta <-
    x %>%
    purrr::pluck("player", 1) %>%
    purrr::lmap(.unlist_and_bind_fn) %>%
    dplyr::bind_cols() %>%
    dplyr::rename_with(
      ~ paste("player", .x, sep = "_"), .cols = !tidyselect::matches("^player_"))

  if(!is.null(pluck_args)){

    subresource_element <-
      purrr::pluck(x, !!!pluck_args)

    fn_todo <-
      rlang::expr(fn(subresource_element))

    data_list$subresource_df <-
      rlang::eval_tidy(fn_todo)
  }

  df <-
    data_list %>%
    purrr::compact() %>%
    purrr::reduce(dplyr::bind_cols)

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

df <-
  x %>%
  purrr::flatten() %>%
  purrr::modify_at("waiver_days", dplyr::bind_rows) %>%
  purrr::modify_at("roster_positions", purrr::flatten_df) %>%
  purrr::modify_at(
    "stat_categories",
    purrr::compose(
      .dir = "forward",
      ~ purrr::pluck(.x, 1),
      ~ purrr::modify_depth(.x, 2, purrr::modify_at, "stat_position_types", purrr::flatten_df),
      ~ purrr::map_df(.x, purrr::flatten_df)
    )
  ) %>%
  purrr::modify_at(
    "stat_modifiers",
    ~purrr::map_df(.x, purrr::flatten_df)
    ) %>%
  purrr::modify_at(
    "divisions",
    purrr::flatten_df
    ) %>%
  purrr::modify_at(
    "week_has_enough_qualifying_days",
    ~ dplyr::bind_rows(.x) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "week", values_to = "value")
  ) %>%
  purrr::modify_if(
    purrr::is_list,
    ~ tidyr::nest(.x, data = tidyselect::everything())
  ) %>%
  purrr::imap(~ purrr::set_names(.x, nm = .y)) %>%
  dplyr::bind_cols()

return(df)

  }


#........................TRANSACTION PARSE.......................


#' Parse transactions resource.
#'
#' This function call .player_parse_fn
#'
#' @param x element to parse
#' @param pluck_args List of arguments passed to purrr::pluck.
#' @param fn Function to run on subresource.
#'
#' @return a tibble
#'
#' @keywords internal
.transaction_parse_fn <- function(x, pluck_args, fn) {

    transaction_meta <-
        x %>%
        purrr::pluck("transaction", 1) %>%
      purrr::lmap(.unlist_and_bind_fn) %>%
      dplyr::bind_cols() %>%
      dplyr::rename_with(
        ~ paste("transaction", .x, sep = "_"), .cols = !tidyselect::matches("^transaction_"))

    # this might be useful if transaction_meta abive doesnt handle trades.

        # rrapply::rrapply(
        #     classes = "list",
        #     condition = function(x, .xname) .xname %in% c("picks"),
        #     f = function(x) purrr::flatten_df(x) %>% tidyr::nest(picks = tidyselect::everything()),
        #     how = "replace"
        # ) %>%
        # dplyr::bind_cols() %>%
        # dplyr::rename_with(
        #         ~ paste("transaction", .x, sep = "_"), .cols = !tidyselect::matches("^transaction_"))

    subresource_element <-
      purrr::pluck(x, !!!pluck_args)

    fn_todo <-
      rlang::expr(fn(subresource_element))

    subresource_df <-
      rlang::eval_tidy(fn_todo)

    df <-
      dplyr::bind_cols(transaction_meta, subresource_df)


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

  df <-
    x %>%
    purrr::pluck("matchup") %>%
    purrr::modify_at("matchup_grades",
                     ~ purrr::flatten_df(.x) %>%
                       purrr::pluck("grade")
  ) %>%
  purrr::modify_at(
    "stat_winners",
    ~ purrr::flatten_df(.x) %>%
      tidyr::nest(stat_winners = tidyselect::everything())
  ) %>%
  purrr::modify_at(
    "0",
    purrr::compose(.dir = "forward",
    ~ purrr::pluck(.x, "teams"),
      function(x) purrr::map_df(
        x,
        .team_resource_parse_fn,
        pluck_args = list("team", 2),
        fn = function(x) .team_stats_parse_fn(x)
      ))
  ) %>%
  dplyr::bind_cols() %>%
  dplyr::rename_with( ~ paste("matchup", .x, sep = "_"),
                      .cols = !tidyselect::matches("^matchup_"))


  return(df)

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ STATS PARSE FUNCTIONS  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#......................TEAM STATS PARSE FN.......................


#' Parse team stats data.
#'
#' @param x Stats sub-resource of teams resource
#'
#' @keywords internal
.team_stats_parse_fn <- function(x) {

  df <-
  x %>%
    purrr::imap(purrr::set_names, nm = ~ paste(.y, .x, sep = "_")) %>%
    purrr::flatten() %>%
    purrr::modify_at(
      "team_stats_stats",
      purrr::compose(
        .dir = "forward",
        ~ purrr::flatten_df(.x),
        ~ tidyr::nest(.x, team_stats = tidyselect::everything())
      )
    ) %>%
    purrr::modify_at(
      "team_remaining_games_total",
      purrr::compose(.dir = "forward",
                     function(x) purrr::set_names(x, ~ paste("team", .x, sep = "_")),
                     ~ purrr::flatten_df(.x))
    ) %>% dplyr::bind_cols()

  # team_stats <-
  #   x %>%
  #   magrittr::extract2("team_stats") %>%
  #   purrr::modify_if(is.list,
  #                    ~ purrr::flatten_df(.x) %>% tidyr::nest(data = tidyselect::everything())) %>%
  #   dplyr::bind_cols() %>%
  #   dplyr::rename_with(~ paste("team_stats", .x, sep = "_"),
  #                      .cols = !tidyselect::matches("^team_stats_"))
  #
  # team_points <-
  #   x %>%
  #   magrittr::extract2("team_points") %>%
  #   purrr::modify_if(is.list,
  #                    ~ purrr::flatten_df(.x) %>% tidyr::nest(data = tidyselect::everything())) %>%
  #   dplyr::bind_cols() %>%
  #   dplyr::rename_with( ~ paste("team_points", .x, sep = "_"),
  #                       .cols = !tidyselect::matches("^team_points_"))
  #
  # team_remaining_games <-
  #   x %>%
  #   magrittr::extract2("team_remaining_games") %>%
  #   purrr::modify_if(is.list, ~ purrr::set_names(.x, nm = ~ paste("total", .x, sep = "_"))) %>%
  #   dplyr::bind_cols() %>%
  #   dplyr::rename_with( ~ paste("remaining_games", .x, sep = "_"),
  #                       .cols = !tidyselect::matches("^total_"))
  #
  # df <-
  #   list(team_stats, team_points, team_remaining_games) %>%
  #   purrr::compact() %>%
  #   purrr::reduce(dplyr::bind_cols)

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
.player_stats_parse <- function(x) {

  coverage <-
    x %>%
    purrr::pluck("0") %>%
    dplyr::bind_cols()


  player_stats <-
    x %>%
    purrr::pluck("stats") %>%
    purrr::map_df(purrr::flatten_df)

  stats <-
    dplyr::bind_cols(coverage, player_stats)

  return(stats)

}

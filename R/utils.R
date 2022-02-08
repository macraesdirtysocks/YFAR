##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                 KEY CHECKS                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#                  SINGLE RESOURCE KEY CHECK                  ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Check a single key for validity
#'
#' This function checks the provided keys for uniformity.  If more that one type of key is detected
#' the function will select the key type appearing most often and removes the rest.
#'
#' This function assigns resource variable.
#' This function alters key argument.
#' This function calls `.key_resource_assign()`
#'
#' @param key Vector of keys
#' @param key_check_fn One of internal .key_check functions.
#' @return A Vector
#' @keywords internal

.single_resource_key_check <- function(key, key_check_fn){

  # Check if keys are type game, remove FALSE and duplicates.
  if(sum(!purrr::map_lgl(key, key_check_fn) > 0)) {

    invalid_key <-
      key[!purrr::map_lgl(key, key_check_fn)]

    cat(crayon::cyan("The following failed", match.call()[3], "and were removed:\n"), sep = " ")
    cat(crayon::cyan(stringr::str_flatten(invalid_key, collapse = "\n")), sep = "\n")

    key <-
      key[purrr::map_lgl(key, key_check_fn)] %>%
      vctrs::vec_unique()

  } else{

    key <-
      key %>%
      vctrs::vec_unique()
  }

  if(vctrs::vec_size(key) < 1){
    stop(message(crayon::cyan("No valid game keys provided.")), call. = FALSE)
  }

  return(key)
}


#                 MULTIPLE RESOURCE KEY CHECK                 ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Check key argument for homogeneity
#'
#' This function checks the provided keys for uniformity.  If more that one type of key is detected
#' the function will select the key type appearing most often and removes the rest.
#'
#' This function assigns resource variable.
#' This function alters key argument.
#' This function calls `.key_resource_assign()`
#'
#' @param key Vector of keys
#' @param e_key_types Vector of eligible key types, default is c("games", "leagues", "teams", "players").
#' @return A list
#' @importFrom sjmisc `%nin%`
#' @keywords internal
.multiple_resource_key_check <- function(key, e_key_types = c("games", "leagues", "teams", "players"))
{
  # Get unique keys
  key_unique <- vctrs::vec_unique(key)

  key_types <-
    purrr::map_chr(key_unique, .key_resource_assign)

  # Count key types present and select type which occurs most frequently and assign to resource.
  resource <-
    key_types[!is.na(key_types)] %>%
    vctrs::vec_count(sort = "count") %>%
    dplyr::filter(key %in% e_key_types) %>%
    vctrs::vec_slice(1) %>%
    dplyr::pull(key)

  # Print message about resource selection above.
  if(vctrs::vec_size(vctrs::vec_unique(key_types)) > 1){
    cat(crayon::cyan$bold("Multiple key types provided", resource, "keys selected by majority\n"), sep = " ")
  }

  key_check_fn <- switch(
    resource,
    "games" = {.game_key_check},
    "leagues" = {.league_key_check},
    "teams" = {.team_key_check},
    "players" = {.player_key_check}
  )

  #Remove keys.
  valid_key <- .single_resource_key_check(key_unique, key_check_fn)

  # valid_key <- vctrs::vec_unique(x[key_validity_check_fn(x)])
  #
  # Find keys not equal to resource, these will be removed.
  invalid_key <- key_unique[key_unique %nin% valid_key]

  arg_list <-
    list(
      resource = resource,
      key = valid_key,
      invalid_key = invalid_key
    )

  return(arg_list)
}



#                     KEY RESOURCE ASSIGN                     ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Key check
#'
#' Detect which key provided to key argument.
#'
#' @param key league_key, team_key, player_key or game_key supplied to y_function
#'
#' @keywords internal
.key_resource_assign <- function(key){

    if (.league_key_check(key) == TRUE) {
        "leagues"
    } else if (.team_key_check(key) == TRUE) {
        "teams"
    } else if (.player_key_check(key) == TRUE){
        "players"
    } else if(.game_key_check(key) == TRUE){
        "games"
    } else {
        # stop(message("please supply a valid game_key, league_key, team_key or player_key"))
      NA_real_
    }
}

#                        GAME KEY CHECK                       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Game key check
#'
#' Check format of game keys
#'
#' @param key game key to check
#'
#' @keywords internal
.game_key_check <- function(key){
    stringr::str_detect(key, pattern = "^[:digit:]{2,4}$")
}


#                      LEAGUE KEY CHECK                       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' League key check.
#'
#' Check format of league keys.
#'
#' @param key league key to check.
#'
#' @keywords internal
.league_key_check <- function(key){
    stringr::str_detect(key, pattern = "\\.l\\.[:digit:]{2,10}$")
}


#                        TEAM KEY CHECK                       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Team key check
#'
#' Check format of team_key
#'
#' @param key team_key to check
#'
#' @keywords internal
.team_key_check <- function(key){
    stringr::str_detect(key, pattern = "\\.t\\.[:digit:]{1,2}$")
}


#                      PLAYER KEY CHECK                       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Player key check
#'
#' Check format of player_keys
#'
#' @param key player_key to check
#'
#' @keywords internal
.player_key_check <- function(key){
    stringr::str_detect(key, pattern = "\\.p\\.[:digit:]{2,10}$")
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              DATE PARAM CHECKS                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#                      DATE PARAM CHECK                       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Date parameter assignment.
#'
#' Check that the date argument provided as argument to YFAR functions
#' is compatible with league.  In other words you can't supply a week integer
#' argument to daily leagues and you can't supply a date argument to weekly nfl leagues.
#'
#' This function is for uri construction.
#'
#' @param game_key game key.
#' @param game_date game_date argument provided to YFAR function.
#'
#' @keywords internal
.date_param_fn <- function(game_key, game_date){

  is_football <- game_key %in% .nfl_game_keys()
  week_date <- game_date[.week_format_check(game_date)]
  ymd_date <- game_date[.date_format_check(game_date)]

  if(is_football & !vctrs::vec_is_empty(week_date)){
    date_param <-
      stringr::str_c("type=week;week=", stringr::str_flatten(week_date, collapse = ","))

  }else if(!is_football & !vctrs::vec_is_empty(ymd_date)){
    date_param <-
      stringr::str_c("type=date;date=", stringr::str_flatten(as.Date(ymd_date, origin = "1970-01-01"), collapse = ","))
  } else{
  NULL
  }
}


#                      DATE FORMAT CHECK                      ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Check argument is date format
#'
#' Checks the argument for "%Y-%m-%d" date format.
#'
#' @param ... What to check.
#'
#' @keywords internal
.date_format_check <- function(x){


    suppressWarnings(!is.na(lubridate::ymd(x)))

}


#                      WEEK FORMAT CHECK                      ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Check argument is week format
#'
#' Checks the argument is an integer between 1 and 18.
#'
#' @param x What to check.
#'
#' @keywords internal
.week_format_check <- function(x){

  suppressWarnings(
    !is.na(as.integer(as.character(x)))
)

}


#                      GAME KEY ASSIGN FN                     ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Assign game key from a vector of keys.
#'
#' @param key A vector of league game or player keys.
#'
#' @return A string
#' @keywords internal
.game_key_assign_fn <- function(key) {

  # Isolate game keys from player keys.
  game_key <-
    stringr::str_extract(key, pattern = "^[:digit:]*")

  # Count unique game keys
  unique_game_key <- vctrs::vec_unique(game_key) %>% vctrs::vec_size()

  # If length unique_game_keys is > 1 take the most common game_key.
  if (unique_game_key > 1) {
    game_key <-
      vctrs::vec_count(game_key, sort = "count") %>%
      vctrs::vec_slice(1) %>%
      dplyr::pull(key)

    cat("Mulitple game keys provided, selecting",
        game_key,
        "by majority.\n",
        sep = " ")

    invisible(game_key)

  } else{
    cat(crayon::cyan("All keys reference game", game_key[1], "\n", sep = " "))
    invisible(vctrs::vec_unique(game_key))
  }
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                    OTHER                                 ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#                  URI PATH PACKER FUNCTION                   ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Divide, group and collapse uri paths.
#'
#' Function creates groups of equal length with the last group containing any remainder.
#'
#' Vector of paths is padded with NA values and passed to matrix for packaging.
#'
#' Function is intended to create an indicies for tibbles.
#'
#' The idea here is to reduce the number of calls to the API.  The API will return a maximum
#' response length of 25.  This function helps create requests that will GET responses
#' of length 25.
#'
#' Example: if you pass 26 player_key's to `y_player_stats()` the function will create 2 uri
#' paths. One with the first 25 players and one for remaining player.
#'
#' These paths are then passed to `httr::build_url()`
#'
#' @param x Number or vector to cut and sequence.
#' @param package_length Desired length of uri path package.
#'
#' @return A numeric vector.
#'
#' @keywords internal
.uri_path_packer <- function(x, package_length = 25){

    x <- if(is.numeric(x)){
        seq_len(x)
        } else {
            x
        }

    # Values in vector passed to function
    vector_head <- x

    # If remainder == 0 no padding required.
    # If remainder != 0 padding values required to for matrix creation.
    if(vctrs::vec_size(x)%%package_length != 0){
        vector_tail <- rep(NA, times = package_length - (vctrs::vec_size(x) %% package_length))
        full_vector <- vctrs::vec_c(vector_head, vector_tail)
    } else{
        full_vector <- vector_head
        }

    uri_paths <-
        # Pass vector to matrix which will divide up the paths
        matrix(full_vector, nrow = package_length) %>%
        # Convert to tibble
        tibble::as_tibble(.name_repair = janitor::make_clean_names) %>%
        as.list() %>%
        purrr::map(stats::na.omit) %>%
        purrr::map_chr(glue::glue_collapse, sep = ",")

    return(uri_paths)
}



#                      SEQ PAGES FUNCTION                     ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Sequence uri page start and count params
#'
#' The Yahoo! API only returns 25 players at a time.  So if you want more than 25 players you
#' need to create a sequence of uri that call 25 players each.
#'
#' I.E. If 50 players are desired you need 2 params:
#' `start=0;count=25` and `start=25;count=25`
#'
#' This function takes a desired number of players (count) and how deep in the player
#' list to start (start) and turns them into a sequence of params with length 25.
#'
#' @param start What count to start at.
#' @param count Number of players to return.
#' @param i Response length.
#'
#' @return A vector of strings.
#' @keywords internal
.seq_pages_fn <- function(start, count, i = 25){

    quotient <- count%/%i
    remainder <- count%%i

    # This is necessary to correct the count if there's no remainder.
    # count = 100 will generate a sequence that includes the number 100.
    # A uri param will then be generated for `start=100;count=25`.
    # So essentially the function returns 25 players more than are asked for.
    if(remainder == 0){
        count <- count-i
        remainder <- NULL
    }

    # Sequence the starts.
    pages_start <-  seq(from = start, to = start+count, by = i)
    # Sequence the counts. Equal to i other than the remainder.
    pages_count <-  vctrs::vec_c(rep(25, quotient), remainder)
    # Glue start and count.
    pages_params <- glue::glue("start={pages_start}", "count={pages_count}" , .sep = ";")
    # Return.
    return(pages_params)
}


#                    CURRENT WEEK FUNCTION                    ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
##                          STAT CATEGORY FUNCTIONS                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#                YAHOO HOCKEY STAT CATEGORIES                 ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
##                                  GAME KEYS                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~
##  ~ NFL  ----
##~~~~~~~~~~~~~

#' NFL game keys
#'
#' Create a vector of NFL game keys
#'
#' @return A Vector
#' @keywords internal
.nfl_game_keys <- function(){
    c("49", "50", "53", "57", "79", "101", "124", "153", "175", "199",
  "222", "242", "257", "273", "314", "331", "348", "359", "371",
  "380", "390", "399", "406")
    }

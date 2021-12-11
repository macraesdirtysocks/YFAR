#' Get team data of a given Yahoo! Fantasy league.
#'
#' Teams in a league make moves, have co-managers, have a waiver prioity etc.  This function
#' returns all team level meta data from the Yahoo! Fantasy API.
#'
#' @param league_id League id as a string in the form "000.l.0000".  League id can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#' @param debug returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a list
#' @export
y_teams <- memoise::memoise(function(league_id = NULL, token_name = NULL, debug = FALSE){

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    resource <- "league"
    subresource <- "teams"
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
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2", resource, league_id, subresource, sep = "/"),
            query = "format=json"
        )

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                GET RESPONSE                              ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    r <-
        .y_get_response(uri, api_token)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                   CONTENT                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    r_parsed <-
        .y_parse_response(r, "fantasy_content", "league", 2, "teams") %>%
        purrr::map(purrr::pluck, "team", 1) %>%
        purrr::keep(purrr::is_list) %>%
        purrr::map(purrr::flatten)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                PARSE CONTENT                             ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    team_meta <-
        r_parsed %>%
        purrr::map(purrr::keep, purrr::negate(purrr::is_list)) %>%
        purrr::map(purrr::modify_if, is.numeric, as.character) %>%
        dplyr::bind_rows()

    team_logos <-
        r_parsed %>%
        purrr::map(purrr::pluck, "team_logos", 1, "team_logo") %>%
        purrr::map_df(dplyr::bind_rows) %>%
        purrr::set_names(~paste("team", ., sep = "_"))

    managers <-
        r_parsed %>%
        purrr::map(purrr::pluck, "managers") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map(purrr::set_names, ~paste(., seq_along(.), sep = "_")) %>%
        purrr::map(dplyr::bind_rows) %>%
        dplyr::bind_rows(.id = "team") %>%
        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), tidyr::replace_na, "0")) %>%
        dplyr::group_nest(team, .key = "manager_info", keep = FALSE) %>%
        dplyr::select(-c(team))


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      DF                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    df <-
        dplyr::bind_cols(
            team_meta,
            team_logos,
            managers
            )


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    RETURN                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    if(!debug){return(df)}

    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                data = df
            ),
            class = "yahoo_fantasy_api")

    return(data_list)
}
)

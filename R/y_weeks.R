#' Title
#'
#' @param game_id a 3 or 4 digit integer.  It is the first 3 digits of league_id.
#' Can be found with y_games().
#' @param token_name Assigned object name used when creating token with y_create_token().
#'
#' @return a list
#' @export
y_weeks <- function(game_id = NULL, token_name = NULL) {

    resource <- "game"
    subresource <- "game_weeks"

    api_token <- token_name

    game_weeks <-


    #.............................Checks.............................


    stopifnot(is.numeric(game_id))
    stopifnot(nchar(game_id) > 2 & nchar(game_id) < 5)
    .token_check(token_name, api_token, name = .GlobalEnv)




    #..............................URI...............................


    uri <-
        httr::modify_url(
            url = "https://fantasysports.yahooapis.com",
            path = paste("fantasy/v2",resource, game_id, subresource, sep = "/"),
            query = "format=json"
        )


    #......................GET response object.......................


    r <-
        .y_get_response(uri, api_token)


    #.........................Parsed reponse.........................

    r_parsed <-
        .y_parse_response(r, "fantasy_content", "game")


    #...........................Empty list...........................


    game_week_list <-
        list(
            league_meta = NULL,
            game_weeks = NULL
            )


    #..........................league meta...........................


    game_week_list$league_meta =
        r_parsed %>%
        purrr::pluck(1) %>%
        dplyr::bind_rows()


    #...........................game weeks...........................


    game_week_list$game_weeks =
        r_parsed %>%
        purrr::pluck(2, "game_weeks") %>%
        purrr::map(purrr::pluck, "game_week") %>%
        purrr::map_df(dplyr::bind_rows) %>%
        dplyr::mutate(matchup_length = difftime(end, start))


    #............................bind df.............................


    df <-
        dplyr::bind_cols(game_week_list$game_weeks, game_week_list$league_meta)


    #.............................return.............................


    data_list <-
        structure(
            list(
                content = r_parsed,
                uri = uri,
                data_list = game_week_list,
                data = df
            ),
            class = "yahoo_fantasy_api")

return(data_list)

}

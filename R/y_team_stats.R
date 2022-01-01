#' Get team level stats for scoring categories
#'
#' returns team or league stats depending on what id (league or team) is supplied to `id`
#'
#' @param id league id or team id as a string in the form "000.l.0000" or "000.l.0000.t.0".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with `y_create_token()`.
#' @param week week of fantasy season to return.  Accepts 3 arguments `current`, `NULL`, `a number`.
#' `current` by default returns current week, null returns aggregated season stats, number returns stats for that week.
#' @param debug returns a list of data such as uri call and content.  Useful for debugging.
#'
#' @return a list
#' @export
y_team_stats <-
    function(id = NULL, token_name = NULL, week = "current", debug = FALSE) {

        api_token <- token_name

        .token_check(token_name, api_token, name = .GlobalEnv)
        stopifnot(is.numeric(week) | is.null(week) | week == "current")


        #.......................RESOURCE VARIABLES.......................

        resource <- .id_check(id)
        subresource <- ifelse(identical(resource, "league"), "teams/stats", "stats")

        #..............................URI...............................

        uri <-
            httr::modify_url(
                url = "https://fantasysports.yahooapis.com",
                path = paste("fantasy/v2",
                             resource,
                             id,
                             subresource,
                             sep = "/"),
                param = glue::glue("type=week;week={week}"),
                query = "format=json"
            )

        #............................RESPONSE............................

        r <-
            .y_get_response(uri, api_token)

        r_parsed <-
            .y_parse_response(r, "fantasy_content")


        #...............................DF...............................


        if (!debug) {
            if (identical(resource, "league")) {

                preprocess <-
                    r_parsed %>%
                    purrr::pluck(2, "teams") %>%
                    purrr::keep(purrr::is_list)

                df <-
                    preprocess %>%
                    purrr::map_df(.stats_data_func)

                return(df)

            } else if (identical(resource, "team")) {

                df <-
                    .stats_data_func(r_parsed)

                return(df)
            } else {
                stop(message(
                    "unknown resource, please supply a league_id or team_id"
                ))
            }
        }


        #.............................RETURN.............................


        data_list <-
            structure(list(
                response = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")

    }

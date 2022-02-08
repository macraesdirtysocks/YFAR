#' Get player stats data from Yahoo! Fantasy API.
#'
#' Want stats for a player or group of players?  Provide a vector of player keys and ye shall receive.
#'
#' Want stats for a specific date? Provide a vector of game dates in form YYYY-MM-DD (%Y-%m-%d).
#'   Default game date NUll will return aggregate stats for the season.
#'   For weekly sports such as nfl you can provide an integer denoting a fantasy week.
#'
#' @param player_key Vector of player keys. Key usually in the form xxx.p.xxxx.
#'    Found with `y_players()`, `y_player_slate()` or `y_rosters()`
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param game_date Date or week of fantasy season.
#'    Default NULL will return aggregate stats for current season.  Accepts a vector of dates.
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble
#' @export
y_player_stats <-
    function(player_key = NULL, token_name = NULL, game_date = NULL, debug = FALSE, quiet = TRUE) {


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    TOKEN                                 ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        api_token <- token_name
        .token_check(token_name, api_token, name = .GlobalEnv)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        resource <- "players"
        subresource <- "stats"
        uri_out <- "player_keys="

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        stopifnot(!is.null(api_token))
        stopifnot(!is.null(player_key))

        # Check if keys are type league, remove FALSE and duplicates.
        key <- .single_resource_key_check(player_key, .player_key_check)

        # Determine what game the player_key belongs to.
        game_key <- .game_key_assign_fn(key)

        if(!quiet){cat(crayon::cyan("game is", game_key), sep = " ")}

        # Subset out player_keys belonging to game_key.
        # Function can't call multiple game resources.
        key <-
            stringr::str_subset(string = key, pattern = game_key)

        # quiet
        if(!quiet){cat(crayon::cyan("Resource is", resource, "\n"), sep = "")}
        if(!quiet){cat(crayon::cyan("Keys are...\n", stringr::str_flatten(key, collapse = "\n")), sep = "\n")}

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Initial uri components
        uri_parsed <- structure(
            list(
                scheme = "https",
                hostname = "fantasysports.yahooapis.com/fantasy/v2",
                port = NULL,
                path = resource,
                query = list(format = "json"),
                params = NULL
            ),
            class = "url"
        )

        # Construct path.
        key_path <-
            .uri_path_packer(key, 25)

        # Construct param
        uri_parsed$params <-
            stringr::str_c(uri_out, key_path, "/", subresource, sep = "")

        # If game_date not null run .date_parm_fn which will check date for compatibility with
        # game key and assign date param.  This essentially makes sure nfl leagues get week= game date
        # parameter.  Else NULL.
        if(!is.null(game_date)){
            date_param <- .date_param_fn(game_key, game_date)
        } else{
            date_param <- NULL
        }

        # If date_param is not null concatenate to end of params.
        # If a game_date is supplied and compatible it will be pasted to the end of the uri params.
        if(!is.null(date_param)) {
            uri_parsed$params <-
                stringr::str_c(uri_parsed$params, date_param, sep = ";")
        }

        uri <-
            httr::build_url(uri_parsed)

        if(!quiet){cat(crayon::cyan("uri is...\n", uri), sep = "\n")}

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        r <-
            purrr::map(uri, .y_get_response, api_token)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                          CHECK RESPONSE FOR ERRORS                       ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(sum(!purrr::map_lgl(r, httr::http_error)) <= 0) {
            stop(message(
                crayon::cyan("All requests returned errors. You may need a token refresh.")), call. = FALSE)
        } else{
            r <- r[!purrr::map_lgl(r, httr::http_error)]
        }

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        r_parsed <-
            purrr::map(r, .y_parse_response, "fantasy_content", resource)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        if (!debug) {

            preprocess <-
                r_parsed %>%
                purrr::flatten() %>%
                purrr::keep(purrr::is_list)

            df <-
                tryCatch(
                    expr =
                        preprocess %>%
                        purrr::map_df(.player_resource_parse_fn, .player_stats_parse),

                    error = function(e) {
                        message(
                            crayon::cyan(
                                "Function failed while parsing games resource with .player_resource_parse_fn. Returning debug list."
                            )
                        )
                    }
                )

            if (tibble::is_tibble(df)) {
                return(df)
            }
        }

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                DEBUG RETURN                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        data_list <-
            structure(list(
                resource= resource,
                response = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")

        return(data_list)

    }

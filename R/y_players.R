#' Get player data from Yahoo! Fantasy API.
#'
#' This function is not intended to get all players in a league.  To get all players in your league use the
#' function `y_player_slate()`.
#'
#' This function is intended to get a subset of players i.e. top 100 players sorted by AR (actual rank).
#'
#' Takes many filter and sort arguments via ...
#' E.G. sort = 1 will return however many players you requested sorted by stat 1.
#' See vignette for all possibilities.
#'
#' @param key Game or league key as a string in the form "000" or 000.l.0000".  Can be found with `y_games()`.
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param number_of_players Number of players to return.  Default is 100
#' @param start Where to start the count in uris i.e. 50.  Default is 0.
#' @param debug Returns a list of data uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#' @param ... Other URI filter arguments.  See vignette for full list.
#' @return A tibble
#' @importFrom zeallot `%<-%`
#' @export
y_players <-
    memoise::memoise(
        function(key = NULL, token_name = NULL, start = 0, number_of_players = 100, quiet = TRUE, debug = FALSE, ...) {


            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ##                                    TOKEN                                 ----
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            # Standardize token name
            api_token <- token_name
            .token_check(token_name, api_token, name = .GlobalEnv)

            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ##                                    CHECKS                                ----
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            stopifnot(!is.null(key))
            stopifnot(!is.null(token_name))

            # Eligible key types.
            e_key_types <- c("games", "leagues")

            # Assign a resource to each key and count.
            # Function then selects most frequently occurring resource and assigns value to resource.
            c(resource, key, .) %<-% .multiple_resource_key_check(key, e_key_types = e_key_types)

            # quiet
            if(!quiet){cat(crayon::cyan("Resource is", resource, "\n"), sep = "")}
            if(!quiet){cat(crayon::cyan("Keys are...\n", stringr::str_flatten(key, collapse = "\n")), sep = "\n")}

            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ##                                  ARGUMENTS                               ----
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            # resource assigned in if statement above.
            subresource <- "players"
            uri_out <-
                switch(resource, "games" = "game_keys=", "leagues" = "league_keys=")

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

            # Page params
            # Again the API only returns 25 at a time so anything over 25 needs to be broken up.
            # Call internal function to sequence start and count numbers into params.
            # I.E. take count = 100, start = 0 into 4 uri's that call 25 players each.
            # https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/players;start=0;count=25?format=json
            pp <-
                .seq_pages_fn(start = start, count = number_of_players, i = 25)

            if (!quiet) {
                cat(crayon::cyan("page parameters are", pp, "\n"), sep = " ")
            }

            page_params <-
                .seq_pages_fn(start = start, count = number_of_players, i = 25)

            # Passed by ... argument
            other_uri_params <- list(...)

            key_path <-
                .uri_path_packer(key)

            # Create paths using .uri_path_package(key) to pack player keys into groups of 25.
            uri_parsed$params <-
                stringr::str_c(uri_out, key_path, "/", subresource, ";", page_params)

            # Modify page params with other uri params supplied to ...
            # If other_uri_params is not empty they are supplied collapse with sep=";" and paste them to the end of pp.
            # Paste ... names to values to make up a param.
            # I.E. take list(sort=1) and turn it into ;sort=1.
            # This is essentially a work-around for glue returning and empty string when ... is empty.
            if (!purrr::is_empty(other_uri_params)) {
                pp <-
                    stringr::str_flatten(stringr::str_c(names(other_uri_params), other_uri_params, sep = "="),
                                         collapse = ";")
                uri_parsed$params <-
                    stringr::str_c(uri_parsed$params, pp, sep = ";")
            }

            # Build uris.
            uri <- httr::build_url(uri_parsed)

            if(!quiet){cat(crayon::cyan("uri is...\n", uri), sep = "\n")}

            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ##                                GET RESPONSE                              ----
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            if (!quiet) {
                cat(crayon::cyan("Getting responses...\n"))
            }

            r <-
                purrr::map(uri, .y_get_response, api_token)

            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ##                                   CONTENT                                ----
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            r_parsed <-
                purrr::map(r, .y_parse_response, "fantasy_content", resource)

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
            ##                                      DF                                  ----
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            if (!debug) {
                if (!quiet) {
                    cat(crayon::cyan("Parsing responses and and returning a tibble...\n"))
                }

                resource_parse_fn <-
                    switch(resource,
                           "games" = {
                               .game_resource_parse_fn
                           },
                           "leagues" = {
                               .league_resource_parse_fn
                           })

                initial_pluck <-
                    switch(resource,
                           "games" = list("game", 2, 1),
                           "leagues" = list("league", 2, 1)
                           )

                preprocess <-
                    r_parsed %>%
                    purrr::flatten() %>%
                    purrr::map(list_pre_process_fn)

                df <-
                    tryCatch(
                        expr =
                            preprocess %>%
                            purrr::map_df(resource_parse_fn, pluck_args = initial_pluck, fn = function(x) purrr::map_df(x, .player_resource_parse_fn)),

                        error = function(e) {
                            message(
                                crayon::cyan(
                                    "Function failed while parsing games resource with resource_parse_fn. Returning debug list."
                                )
                            )
                        }
                    )

                if (tibble::is_tibble(df)) {return(df)}

            }


            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ##                                    RETURN                                ----
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


            data_list <-
                structure(list(
                    uri = uri,
                    resource = resource,
                    response = r,
                    r_parsed = r_parsed
                ),
                class = "yahoo_fantasy_api")

            return(data_list)

        })

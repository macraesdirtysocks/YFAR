#' Get roster data from Yahoo! Fantasy API.
#'
#' Supply a vector of team or league keys to the function to retrieve roster data.  If a date
#' is supplied the roster data for that date will be returned.
#'
#' Function accepts a vector of dates.
#'
#' Function is memoised
#'
#' @param key League key or team key as a string in the form "000.l.0000" or "000.l.0000.t.0".  These keys can be found with `y_games()` and `y_teams()`.
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param game_date A vector of dates of fantasy season in form YYYY-MM-DD to return.
#'     Default is null and will return current date.
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble.
#' @importFrom zeallot `%<-%`
#' @export
y_rosters <- memoise::memoise(

    function(key = NULL, token_name = NULL, game_date = NULL, debug = FALSE, quiet = TRUE){


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    TOKEN                                 ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        api_token <- token_name
        .token_check(token_name, api_token, name = .GlobalEnv)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        stopifnot(!is.null(key))
        stopifnot(!is.null(token_name))

        # Eligible key types.
        e_key_types <- c("leagues", "teams")

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
        subresource <- switch(resource, "leagues" = "teams", "teams" = NULL)
        collection = "roster"
        uri_out <- switch(resource,"leagues" = "league_keys=", "teams" = "team_keys=")
        game_key <- .game_key_assign_fn(key)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

        # Pack key paths.
        key_path <- .uri_path_packer(key, 25)

        # Inital params.
        uri_parsed$params <- stringr::str_c(uri_out, key_path, sep = "")

        # Check game date validity.
        game_date_checked <-
            if(!is.null(game_date)){
            game_date[.date_format_check(game_date)]
            } else{
                NULL
            }

        # Update uri params conditional on resource.
        if(identical(resource, "leagues")){
            uri_parsed$params <- stringr::str_c(uri_parsed$params, subresource, collection, sep = "/")
        } else if(identical(resource, "teams")){
            uri_parsed$params <- stringr::str_c(uri_parsed$params, collection, sep = "/")
        } else{
            uri_parsed$params <- uri_parsed$params
        }

        # If game_date_checked is not null create date_params and append to params,
        if(!is.null(game_date_checked) & !vctrs::vec_is_empty(game_date_checked)){
            date_param <- stringr::str_c("type=date;date", game_date_checked, sep = "=")
            uri_parsed$params <- stringr::str_c(uri_parsed$params, date_param, sep = ";")
        }

        # Build uri.
        uri <- httr::build_url(uri_parsed)

        if(!quiet){cat(crayon::cyan("uri is...\n", uri), sep = "\n")}

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        r <-
            purrr::map(uri, .y_get_response, api_token)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                          CHECK RESPONSE FOR ERRORS                       ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(sum(!purrr::map_lgl(r, httr::http_error)) <= 0){
            stop(message(crayon::cyan("All requests returned errors. You may need a token refresh.")), call. = FALSE)
        }

        r <- r[!purrr::map_lgl(r, httr::http_error)]

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        r_parsed <-
            purrr::map(r, .y_parse_response, "fantasy_content", resource)


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!debug) {

            # Preprocess r_parsed.
            preprocess <-
                r_parsed %>%
                purrr::flatten() %>%
                purrr::keep(purrr::is_list)

            # If resource equal to "leagues" .league_resource_parse_fn needed.
            # Otherwise only .team_resource_parse_fn is needed.
            if (identical(resource, "leagues")) {

                df <-
                    tryCatch(
                        expr =
                purrr::map_df(
                        preprocess,
                        .league_resource_parse_fn,
                        .team_resource_parse_fn,
                        .roster_resource_parse_fn
                    ),

                error = function(e) {
                    message(crayon::cyan(
                        "Function failed while parsing games resource with .league_resource_parse_fn. Returning debug list."))
                }
                    )

                if(tibble::is_tibble(df)) {return(df)}

            } else if (identical(resource, "teams")) {

                df <-
                    tryCatch(
                        expr =
                            purrr::map_df(preprocess, .team_resource_parse_fn, .roster_resource_parse_fn),

                        error = function(e) {
                            message(crayon::cyan(
                                "Function failed while parsing games resource with .team_resource_parse_fn. Returning debug list."))
                        }
                    )

                if(tibble::is_tibble(df)) {return(df)}

            } else{
                message(crayon::cyan("Bad resource, can't parse.  Returning debug list."))
            }

        }

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                DEBUG RETURN                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        data_list <-
            structure(
                list(
                    resource = resource,
                    response = r,
                    content = r_parsed,
                    uri = uri
            ),
            class = "yahoo_fantasy_api")

        return(data_list)
    }

)

#' Get team scoring stats from Yahoo! Fantasy API.
#'
#' Returns team or league stats depending on what type of key is
#'   provided as an argument to key.
#'
#' @param key League key or team key as a string in the form "000.l.0000" or "000.l.0000.t.0".
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param week Week of fantasy season to return.
#'   -Accepts 3 arguments:
#'     - `current` returns current week of season.
#'     - An integer corresponding to a week of season.
#'     - If NULL will return aggregated season stats.
#' @param debug returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#' @return A tibble
#' @export
y_team_stats <-
    function(key = NULL, token_name = NULL, week = NULL, debug = FALSE, quiet = TRUE) {


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    TOKEN                                 ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        api_token <- token_name
        .token_check(token_name, api_token, name = .GlobalEnv)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        stopifnot(!is.null(key))
        stopifnot(!is.null(api_token))

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

        if(!quiet){cat(crayon::cyan("Resource is", resource, "\n"), sep = "")}
        if(!quiet){cat(crayon::cyan("Keys are...\n", stringr::str_flatten(key, collapse = "\n")), sep = "\n")}

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # resource assigned in if statement above.
        subresource <- switch(resource, "leagues" = "teams", "teams" = NULL)
        collection <- "stats"
        uri_out <- switch(resource, "leagues" = "league_keys=", "teams" = "team_keys=")

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

        # Params
        key_path <-
            .uri_path_packer(key)

        uri_parsed$params <-
            stringr::str_c(uri_out, key_path, sep = "")

        if(!is.null(subresource)){
            uri_parsed$params <- stringr::str_c(uri_parsed$params, subresource, collection, sep = "/")
        } else{
            uri_parsed$params <- stringr::str_c(uri_parsed$params, collection, sep = "/")
        }

        # If week is not empty tunr it into a param by pasting the name to the value and
        # gluing to already existing param.
        # i.e. week <- list(week=1) becomes week=1 and then type=week;week=1.
        if(!is.null(week)){
            week <- suppressWarnings(week[!is.na(as.integer(as.character(week)))]) %>% vctrs::vec_unique()
            week_param <- stringr::str_c("type=week;week=", week)
            uri_parsed$params <- stringr::str_c(uri_parsed$params, week_param, sep = ";")
        }

        # Build uris.
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
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        r_parsed <-
            purrr::map(r, .y_parse_response, "fantasy_content", resource)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (!debug) {

            # Defining parse_fn is necessary because the team stats resource is not in a list.
            # As soon as you drill down into team[[2]] the team stats are there as elements so the parse functions
            # can be applied directly without mapping in.  It's annoying honesty.  Feeding the parse functions
            # to .league_parse_fn will cause the .team_stats_data_func to map which won't work here.
            parse_fn <-
                function(x) {
                    dplyr::bind_cols(
                        .team_meta_parse_fn(x),
                        .team_stats_parse_fn(x)
                        )
                }

            # Preprocess r_parsed.
            preprocess <-
                r_parsed %>%
                purrr::flatten()

            if (identical(resource, "leagues")) {

                  df <-
                      tryCatch(
                          expr =
                              preprocess %>%
                              purrr::map_df(.league_resource_parse_fn, parse_fn),

                          error = function(e) {
                              message(crayon::cyan(
                                  "Function failed while parsing leagues resource with .league_resource_parse_fn. Returning debug list."))
                          }
                      )

                  if(tibble::is_tibble(df)){return(df)}

            } else if (identical(resource, "teams")) {

               df <-
                   tryCatch(
                       expr =
                           preprocess %>%
                           purrr::map_df(parse_fn),

                       error = function(e) {
                           message(crayon::cyan(
                               "Function failed while parsing teams resource with parse_fn. Returning debug list."))
                           }
                   )

               if(tibble::is_tibble(df)){return(df)}

            } else {
                message(crayon::cyan("Unknown resource, please supply a valid league or team key.  Returning debug list."))
            }
        }

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                DEBUG RETURN                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        data_list <-
            structure(list(
                resource = resource,
                response = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")

        return(data_list)

    }

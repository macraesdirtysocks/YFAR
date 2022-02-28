#' Get match-up stats and results data from Yahoo! Fantasy API
#'
#' Returns match-up stats and results for a given teams and weeks.
#'   Option to provide a week agrument which will return data for that week.
#'   Default NULL will return all past and current weeks of season.
#'   Vector of weeks also accepted.
#'
#' Only past or current match-ups will be returned.
#'
#' @param team_key A string in the form "000.l.0000.t.00".  Team key can be found with `y_teams()`.
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param week Week of fantasy season to return. Default NULL will return all past and current weeks of season.
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble
#' @export
y_matchups <-
    function(team_key = NULL, token_name = NULL, week = NULL, debug = FALSE, quiet = TRUE) {

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    TOKEN                                 ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Standardize token name.
        api_token <- token_name
        .token_check(token_name, api_token, name = .GlobalEnv)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        resource <- "teams"
        subresource <- "matchups"
        uri_out <- "team_keys="

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        stopifnot(!is.null(api_token))
        stopifnot(!is.null(team_key))

        # Check if keys are type league, remove FALSE and duplicates.
        key <- .single_resource_key_check(team_key, .team_key_check)

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

        # Create week param ";week=1,2,3".  NULL will eliminate ";week" and all completed weeks will be returned.
        key_path <- .uri_path_packer(key)

        # Create paths using .uri_path_package(key) to pack player keys into groups of 25.
        uri_parsed$params <- stringr::str_c(uri_out, key_path, "/", subresource, sep = "")

        # Check game date validity.
            if(!is.null(week)){
                week_checked <- week[.week_format_check(week)] %>% vctrs::vec_unique()
            } else{
                week_checked <- NULL
            }

        if(!is.null(week_checked) & !vctrs::vec_is_empty(week_checked)){
            week_param <- stringr::str_c("type=week;weeks=",stringr::str_flatten(week_checked, collapse = ","))
            uri_parsed$params <- stringr::str_glue(uri_parsed$params, week_param, .sep = ";")
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
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!debug) {

            # General preprocess steps that most functions use.
            preprocess <-
                r_parsed %>%
                purrr::flatten() %>%
                purrr::keep(purrr::is_list) %>%
                list_pre_process_fn()

            # If response is form a non H2H league the mathcup elements will contain an error message.
            # Extract error elements and create a message for printing.
            error_elements <-
                preprocess %>%
                rrapply::rrapply(
                    classes = "list",
                    condition = function(x, .xname) .xname == "exception",
                    f = function(x) purrr::pluck(x, "message"),
                    how = "melt"
                ) %>%
                dplyr::select("L1", "value") %>%
                dplyr::mutate(message = stringr::str_c("Error in", L1, "with message:", value, sep = " "))

            # If errors exist print.
            if(nrow(error_elements) > 0){
            message(crayon::cyan$bold("Matchups removed:\n", paste0(error_elements$message, "\n")))
            }

            # Non error list elelments.
           good_elements <- dplyr::setdiff(names(preprocess), error_elements %>% dplyr::pull("L1"))

           # Subset good elements and pluck matchup data.
           preprocess <-
               preprocess[good_elements] %>%
               purrr::map(purrr::pluck, "team", 2, "matchups")


            df <-
                tryCatch(
                    expr =
                        preprocess %>%
                        purrr::map_depth(2, .matchup_parse_fn) %>%
                        purrr::map_df(dplyr::bind_rows),

                    error = function(e) {
                        message(crayon::cyan(
                            "Function failed while parsing games resource with .matchup_parse_fn. Returning debug list."))
                    }
                )

            if(tibble::is_tibble(df)){return(df)}
        }


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                DEBUG RETURN                              ----
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

    }

#' Get team level meta data from Yahoo! Fantasy API.
#'
#' Teams in a league make moves, have co-managers, have a waiver prioity etc.  This function
#' returns all team level meta data from the Yahoo! Fantasy API.
#'
#' @param league_key League key as a string in the form "000.l.0000".  League id can be found with `y_games()`.
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble.
#' @export
y_teams <- memoise::memoise(function(league_key = NULL, token_name = NULL, debug = FALSE, quiet = TRUE){


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    TOKEN                                 ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    api_token <- token_name
    .token_check(token_name, api_token, name = .GlobalEnv)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    resource <- "leagues"
    subresource <- "teams"
    uri_out <- "league_keys="

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    CHECKS                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Check if keys are type league, remove FALSE and duplicates.
    key <- .single_resource_key_check(league_key, .league_key_check)

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

    uri_path <-
        .uri_path_packer(key, 25)

    uri_parsed$params <-
        stringr::str_c(uri_out, uri_path, "/", subresource)

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
    ##                                      DF                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if(!debug){

        preprocess <-
            r_parsed %>%
            purrr::flatten() %>%
            list_pre_process_fn()

        df <-
            tryCatch(
                expr =
                    preprocess %>%
                    purrr::map_df(
                        .league_resource_parse_fn,
                        pluck_args = list("league", 2, 1),
                        fn = function(x)
                            purrr::map_df(x, .team_resource_parse_fn)
                    ),

                error = function(e) {
                    message(
                        crayon::cyan(
                            "Function failed while parsing with .league_resource_parse_fn. Returning debug list."
                        )
                    )
                }
            )

        if(tibble::is_tibble(df)){return(df)}

    }

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                DEBUG RETURN                              ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    data_list <-
        structure(
            list(
                resource = resource,
                r_parsed = r,
                content = r_parsed,
                uri = uri
            ),
            class = "yahoo_fantasy_api")

    return(data_list)
}
)

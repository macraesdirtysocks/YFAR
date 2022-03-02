#' Get standings data from Yahoo! Fantasy API.
#'
#' Supply a team key or vector of team keys to return standings data.
#' This function is best use with y_teams to supply a vector to team keys from a league.
#' It seems you should be able to supply a league key to this function but for some reason the request returns
#' a bunch of team stats data and I wanted to keep that in the `y_team_stats()` function.
#'
#' This function does not return individual category win data form H2H leagues.
#'
#' @param team_key A vector of team keys as a string in the form 000.l.0000.t.0".
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble.
#' @export
y_standings <- function(team_key = NULL, token_name = NULL, debug = FALSE, quiet = TRUE) {


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    TOKEN                                 ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    api_token <- token_name
    .token_check(token_name, api_token, name = .GlobalEnv)

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    resource <- "teams"
    subresource <- "standings"
    uri_out <- "team_keys="

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    CHECKS                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    stopifnot(!is.null(team_key))
    stopifnot(!is.null(token_name))

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

    key_paths <-
        .uri_path_packer(key, 25)

    uri_parsed$params <-
        stringr::str_c(uri_out, key_paths, "/", subresource, sep = "")

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
    ##                                FUNCTION DEFS                             ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    subresource_parse_fn <- function(x){

        atomic <-
            x %>%
            purrr::keep(purrr::is_atomic) %>%
            dplyr::bind_cols()

        the_lists <-
            x %>%
            purrr::keep(purrr::is_list) %>%
            purrr::imap_dfc(~purrr::set_names(.x, nm = paste(.y, names(.x), sep = "_")) %>% purrr::flatten_dfr())

        df <- dplyr::bind_cols(atomic, the_lists)

        return(df)
    }

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                      DF                                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if(!debug) {

        preprocess <-
            r_parsed %>%
            purrr::flatten() %>%
            list_pre_process_fn()


        df <-
            tryCatch(
                expr =
                    purrr::map_df(
                        preprocess,
                        .team_resource_parse_fn,
                        list("team", 2),
                        function(x) purrr::map_df(x, subresource_parse_fn)
                    ),

                error = function(e) {
                    message(
                        crayon::cyan(
                            "Function failed while parsing games resource with .team_resource_parse_fn. Returning debug list."
                        )
                    )
                }
            )

            if(tibble::is_tibble(df)) {return(df)}
    }

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                DEBUG RETURN                              ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    data_list <-
        structure(
            list(
                uri = uri,
                resource = resource,
                response = r,
                r_parsed = r_parsed
            ),
            class = "yahoo_fantasy_api")

    return(data_list)

}

#' Get draft results data from Yahoo! Fantasy API
#'
#' This function returns the draft results for a league or team key.  Function accepts a vector of keys
#'   however they cannot be mixed.  If a mixed type of key is provided the function will select the majority
#'   key type and query the uri using those keys.
#'
#' Oddly the only reference to the players this resource returns are the player keys and player ids so
#'   this function is best used in tandem with `y_players()` or having a local copy of a league's
#'   player slate for joining player names.
#'
#' @param key A vector of league or team keys as strings in the form "000.l.0000" or "000.l.0000.t.0".
#'   Keys can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with `y_create_token()`.
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#' @return A tibble
#' @importFrom zeallot `%<-%`
#' @examples
#' # Not run
#' # y_draft_results("411.l.1239", my_token)
#'
#' @export
y_draft_results <- memoise::memoise(
    function(key = NULL, token_name = NULL, debug = FALSE, quiet = TRUE) {


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
    stopifnot(!is.null(api_token))

    # Eligible key types.
    e_key_types <- c("leagues", "teams")

    # Assign a resource to each key and count.
    # Function then selects most frequently occurring resource and assigns value to resource.
    c(resource, key, .) %<-% .multiple_resource_key_check(key, e_key_types = e_key_types)

    # quiet
    if(!quiet){
        cat(crayon::cyan("Resource is", resource, "\n"), sep = " ")
        cat(crayon::cyan("Keys are...\n", stringr::str_flatten(key, collapse = "\n")), sep = "\n")
    }

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # resource assigned above.
    subresource <- switch(resource, "leagues" = "teams", "teams" = NULL)
    collection <- "draftresults"
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

    # Pack paths into lengths of 25.
    key_path <-
        .uri_path_packer(key, 25)

    # Initial uri params.
    uri_parsed$params <-
        stringr::str_c(uri_out, key_path, sep = "")

    # Other params condition on resource.
    if(!is.null(subresource)){
        uri_parsed$params <- stringr::str_c(uri_parsed$params, subresource, collection, sep = "/")
    } else{
        uri_parsed$params <- stringr::str_c(uri_parsed$params, collection, sep = "/")
    }

    # Build uris.
    uri <- httr::build_url(uri_parsed)

    # quiet
    if(!quiet){cat(crayon::cyan("uri generated...\n", uri, "\n"))}

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

    if(!debug) {

        # Preprocess r_parsed.
        preprocess <-
            r_parsed %>%
            purrr::flatten() %>%
            list_pre_process_fn()

        #.....................If resource == leagues.....................

        if (identical(resource, "leagues")) {

            df <-
                tryCatch(
                    expr =
                        preprocess %>%
                        purrr::map_df(
                            .league_resource_parse_fn,
                            pluck_args = list("league", 2, 1),
                            fn = function(x) purrr:::map_df(x,
                                                            .team_resource_parse_fn,
                                                            pluck_args = list("team", 2, 1),
                                                            fn = function(x) purrr::map_df(x, .unlist_and_bind_fn))
                        ),

                    error = function(e) {
                        message(crayon::cyan(
                            "Function failed while parsing games resource with .league_resource_parse_fn. Returning debug list."))
                    }
                )

            if(tibble::is_tibble(df)){return(df)}


            #......................If resource == teams......................


        } else if (identical(resource, "teams")) {

            df <-
                tryCatch(
                    expr =
                        preprocess %>%
                        purrr::map_df(
                            .team_resource_parse_fn,
                            list("team", 2, 1),
                            fn = function(x) purrr::map_df(x, .unlist_and_bind_fn)
                        ),

                    error = function(e) {
                        message(crayon::cyan(
                            "Function failed while parsing games resource with .team_resource_parse_fn. Returning debug list."))
                    }
                )

            if(tibble::is_tibble(df)){return(df)}


            #..............................Else..............................


        } else {
            message(crayon::cyan("Can't determine resource. Returning debug list."))
        }
    } # Close debug.

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
                class = "yahoo_fantasy_api"
            )

        return(data_list)

}
)

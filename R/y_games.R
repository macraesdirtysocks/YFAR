#' Get user game data from Yahoo! Fantasy API
#'
#' Returns a tibble containing descriptive meta data for all Yahoo! fantasy
#'    games and leagues participated in by the logged in user.  Data returned
#'    describes both past and current games and includes but is not limited to
#'    game codes, year, scoring type and league name.
#'
#' Function is memoised
#'
#' @param token_name Name used for assignment when creating token object with y_create_token().
#' @param debug returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Toggle function messages.
#'
#' @return A tibble
#' @export
y_games <-
    memoise::memoise(function(token_name = NULL, debug = FALSE, quiet = TRUE) {


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    TOKEN                                 ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Standardize token name.
        api_token <- token_name
        .token_check(token_name, api_token, name = .GlobalEnv)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                  ARGUMENTS                               ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        resource <- "users"
        subresource <- "games"
        collection <- "leagues"

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        stopifnot(!is.null(api_token))

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                     URI                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        uri <-
            httr::build_url(structure(
                list(
                    scheme = "https",
                    hostname = "fantasysports.yahooapis.com",
                    port = NULL,
                    path = stringr::str_c("fantasy/v2", resource, sep = "/"),
                    params = stringr::str_c("use_login=1", subresource, collection, sep = "/"),
                    query = list(format = "json"),
                    params = NULL
                ),
                class = "url"))

        if(!quiet){cat(crayon::cyan("uri is...\n", uri, "\n"))}

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                GET RESPONSE                              ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!quiet){cat(crayon::cyan("Sending GET request...\n"))}

        r <-
            .y_get_response(uri, api_token)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                   CONTENT                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!quiet){cat(crayon::cyan("Getting content from response\n"))}

        r_parsed <-
            .y_parse_response(r, "fantasy_content", resource)

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                PARSE CONTENT                             ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                      DF                                  ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(!debug) {

            preprocess <-
                r_parsed %>%
                purrr::pluck("0", "user", 2, "games") %>%
                purrr::keep(purrr::is_list) %>%
                purrr::compact()

            # Subset out special tournaments and promotional leagues i.e. free NFL money leagues and golf.
            preprocess <- preprocess[purrr::map_depth(preprocess, 2, purrr::every, purrr::is_list) %>% purrr::map_lgl(1)]

            if(!quiet){cat(crayon::cyan("Parsing content with .y_games_parse\n"))}

            df <-
                tryCatch(
                    expr =
                        purrr::map_df(preprocess, .game_resource_parse_fn, .league_meta_parse_fn),

                    error = function(e) {
                        message(crayon::cyan(
                            "Function failed while parsing games resource with .game_resource_parse_fn. Returning debug list."))
                    }
                    )

            if(tibble::is_tibble(df)){return(df)}
        }

        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    RETURN                                ----
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

    })

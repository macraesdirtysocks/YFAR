#' Get scoreboard stats and results data from Yahoo! Fantasy API.
#'
#' This function gets scoreboard stats and results for a given league and week.
#' It is similar to `y_matchups()` but takes a league key as opposed to a team key and
#' returns league wide match-up data.
#'
#' `y_scoreboard()` takes a league key and returns match-up data for all teams in requested week.
#' `y_matchups()` takes a team key and returns match-up data for requested weeks and team.
#'
#' @param league_key league as a string in the form "000.l.0000".  League key can be found with `y_games()`.
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param week A integer referring to a week of fantasy season to return.
#'     Default NULL will return current week of season.
#' @param debug Returns a list of data uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble
#' @export
y_scoreboard <- function(league_key = NULL, token_name = NULL, week = NULL, debug = FALSE, quiet = TRUE) {


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    TOKEN                                 ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    api_token <- token_name
    .token_check(token_name, api_token, name = .GlobalEnv)

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    resource <- "leagues"
    subresource <- "scoreboard"
    uri_out <- "league_keys="

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    CHECKS                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Check if keys are type league, remove FALSE and duplicates.
    key <- .single_resource_key_check(league_key, .league_key_check)

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

    # Pack league keys into uri's of length 25.
    key_paths <-
        .uri_path_packer(key, 25)

    uri_parsed$params <-
        stringr::str_c(uri_out, key_paths, "/", subresource, sep = "")

    if(!is.null(week)){
        week_checked <-
            week[.week_format_check(week)] %>% vctrs::vec_unique()
    } else{
        week_checked <- NULL
    }

    # If week is not empty turn it into a param by pasting the name to the value and
    # gluing to already existing param.
    # i.e. week <- list(week=1) becomes week=1 and then type=week;week=1.

    if(!is.null(week_checked) & !vctrs::vec_is_empty(week_checked)){
        week_param <- stringr::str_c("type=week;week=", week_checked)
        uri_parsed$params <- stringr::str_c(uri_parsed$params, week_param, sep = ";")
    }

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

        # General preprocess steps that most functions use.
        preprocess <-
            r_parsed %>%
            purrr::flatten() %>%
            purrr::map(list_pre_process_fn)

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

        # Subset good elements and pluck scoreboard data.
        preprocess <-
            preprocess[good_elements]
            # purrr::map_depth(2, purrr::map_at, 2, ~purrr::pluck(.x, "scoreboard" ,"0", "matchups"))

        df <-
            tryCatch(
                expr =
                    purrr::map_df(
                        preprocess,
                        .league_resource_parse_fn,
                        list("league", 2, "scoreboard" ,"0", "matchups"),
                        function(x) purrr::map_df(x, .matchup_parse_fn)),

                error = function(e) {
                    message(crayon::cyan(
                        "Function failed while parsing games resource with .matchup_parse_fn. Returning debug list."))
                }
            )

        if(tibble::is_tibble(df)){return(df)}

    }

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                    RETURN                                ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    data_list <-
        structure(
            list(
                uri = uri,
                resource = resource,
                response= r,
                content = r_parsed
            ),
            class = "yahoo_fantasy_api")


    return(data_list)

}

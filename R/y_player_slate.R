#' Retrieve the full slate of players for provided league id
#'
#' This function is not intended to get a subset of players, i.e. top 100 players.
#' Use `y_players()` for that case.
#'
#' Note: this function uses janitor::make_clean_names and as a result is a bit slow.
#'
#' @param league_id League id as a string in the form "000.l.0000".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with `y_create_token()`.
#' @param debug Print uri and page counts to console as functions runs.  Useful for debugging.
#' @param ... URI filter arguments passed onto internal .uri_gen_func.
#'
#'
#' @return A tibble
#' @export
y_player_slate <- memoise::memoise(function(league_id, token_name, debug = FALSE, ...) {


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    resource <- "league"
    subresource <- "players"
    api_token <- token_name


    #...........................empty list...........................


    resp_list <- list(uri = NULL,
                      resp_200 = NULL,
                      resp_error = NULL)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                              function - part 1                           ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # The Yahoo! api will return a max of 25 players per request and total number of
    # players in the full player slate is not known.  This loop will continue to get
    # the next 25 players as long as the the previous response contained 25 players.

    ##~~~~~~~~~~~~~~~~~~~~
    ##  ~ while loop  ----
    ##~~~~~~~~~~~~~~~~~~~~

    #......................while loop conditions.....................

    page_start <- 0
    count <- 25

    while (count == 25) {

        #..............................uri...............................

        uri <-
            .uri_gen_func(
                start = page_start,
                number_of_players = 25,
                resp_len = 25,
                league_id = league_id,
                resource = resource,
                subresource = subresource,
                ...
            )


        resp_list$uri <-
            append(resp_list$uri, uri, after = length(resp_list$uri))

        #...............print uri and page for GET request...............


        # If something isn't working set debug = TRUE to print uri and page

        if (debug) {
            print(uri)
            print(page_start)
        }


        #..............................GET...............................


        r <-
            .y_get_response(uri, api_token)


        ##~~~~~~~~~~~~
        ##  ~ if  ----
        ##~~~~~~~~~~~~


        # Divert the response based on http code.
        # if response code == 200 the response is parsed and appended to
        # resp_list$resp_200.
        # else (usually http error 400) the page number is appended to
        # resp_list$resp_error


        if (identical(httr::status_code(r), 200L)) {

            #..........................get content...........................

            r_parsed <-
                .y_parse_response(r, "fantasy_content", resource, 2, subresource)

            # assign response to list element with corresponding page start

            resp_list[["resp_200"]][[as.character(page_start)]] <-
                r_parsed

            #..........................update count..........................

            count <- length(r_parsed)

            if (debug) {
                print(count)
            }

            ##~~~~~~~~~~~~~~
            ##  ~ else  ----
            ##~~~~~~~~~~~~~~

            # append page that produced the http error to "resp_error"
            # because the response cannot be parsed we need to assume there is
            # an additional page to get and manually set count <- 25 or while
            # loop will terminate

        } else {
            # assign response to list element with corresponding page start
            resp_list[["resp_error"]] <-
                append(resp_list[["resp_error"]], page_start)

            #..........................update count..........................

            count <- 25

        }

        #......................increment while loop......................

        page_start <- page_start + 25

    }


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                              function - part 2                           ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # If there is an error in a response it will prevent the
    # function from retrieving the entire 25 player block in the response.  This creates
    # a 25 player gap in the player slate when often it is a single element producing the error.
    # The pages that produce errors get added to resp_list$resp_error.  Then the error pages
    # are sequenced out by 1 so only the one element producing the error is omitted.

    # example: page beginning at 25 returns response code 400. This means players in the
    # response would be 25:49 and they will be missing from the player slate.  25
    # is added to resp_list$resp_error and eventually sequenced out 25:49 and a GET request
    # is generated for each of them in order to flesh out which number is producing the error.

    # This can be seen in the uri construction param resp_len=1 vs resp_len=25.


    #......................sequence error pages......................


    error_pages <-
        resp_list$resp_error


    if (debug) {
        cat("pages",
            error_pages,
            "returned an http error, retrying",
            "\n")
    }


    #..............................uri...............................


    uri <-
        purrr::map(
            error_pages,
            .uri_gen_func,
            start = error_pages,
            number_of_players = 25,
            resp_len = 1,
            league_id = league_id,
            resource = resource,
            subresource = subresource,
            ...
        ) %>%
        purrr::flatten_chr()

    if (debug) {
        print(uri)
    }


    #..............................GET...............................


    reget_response <-
        purrr::map(uri, .y_get_response, api_token)


    #............filter out responses that return errors.............


    reget_response <-
        reget_response[!purrr::map_lgl(reget_response, httr::http_error)]


    #..............................parse.............................


    resp_list[["resp_200"]] <-
        purrr::map(reget_response,
                   .y_parse_response,
                   "fantasy_content",
                   resource,
                   2,
                   subresource) %>%
        append(resp_list[["resp_200"]], ., after = length(resp_list[["resp_200"]]) +
                   1)


    #...............................df...............................


    if(!debug){

        cat("parsing", length(resp_list$resp_200), "responses...", "\n")

        player_list <- resp_list$resp_200 %>% purrr::flatten()

        pb <- progress::progress_bar$new(total = length(player_list))

        player_parse <- function(x){
            pb$tick()
            df <- .player_parse_fn(x)
            return(df)
        }

        df <-
            purrr::map_df(player_list, player_parse)

        return(df)
    }


    #.............................return.............................

    data_list <-
        structure(
            list(
                content = resp_list$resp_200,
                uri = resp_list$uri,
                error_pages = resp_list$resp_error
            ),
            class = "yahoo_fantasy_api"
        )

    return(data_list)

})

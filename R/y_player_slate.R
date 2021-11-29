#' Retrieve the full slate of players for provided league id
#'
#' This function is not intented to get a subset of players, i.e. top 100 players.
#' Use `y_players()` for that case.
#'
#' @param league_id league_id as a string in the form "000.l.0000".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Assigned object name used when creating token with `y_create_token()`.
#' @param debug Print uri and page counts to console as functions runs.  Useful for debugging.
#' @param ... Arguments sort and status passed onto internal .uri_gen_func.
#'
#'
#' @return A list
#' @export
y_player_slate <- memoise::memoise(function(league_id, token_name, debug = FALSE, ...) {
    #.......................function arguments.......................


    api_token <- token_name
    resource <- "league"
    subresource <- "players"


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
                resource,
                league_id,
                subresource,
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
            r_parsed <-
                .y_parse_response(r, "fantasy_content", resource, 2)

            resp_list[["resp_200"]][[as.character(page_start)]] <-
                r_parsed

            count <- purrr::pluck(r_parsed, "players", "count")

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
            resp_list[["resp_error"]] <-
                append(resp_list[["resp_error"]], page_start)
            count <- 25

        }

        #......................increment while loop......................

        page_start <- page_start + 25

    }


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                              function - part 2                           ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # If there is an error in a response it will prevent the
    # function from retrieving the entrire 25 player block in the response.
    # Often it is a single element producing the error so the pages that produce
    # errors get added to resp_list$resp_error.  Then the error pages are sequenced out by 1
    # so only the one element producing the error is omitted.

    # example: page beginning at 25 returns response code 400. This means players in the
    # response would be 25:49 and they will be missing from the player slate.  25
    # is added to resp_list$resp_error and eventually sequenced out 25:49 and a GET request
    # is generated for each of them in order to flesh out which number is producing the error.

    # This can be seen in the uri construction param count=1 vs count=25 above.


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
            number_of_players = 25,
            resp_len = 1,
            "league",
            "411.l.1245",
            "players",
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
                   2) %>%
        append(resp_list[["resp_200"]], ., after = length(resp_list[["resp_200"]]) +
                   1)


    #...............................df...............................


    df <- purrr::map_df(resp_list$resp_200, .player_parse_fn)


    #.............................return.............................

    data_list <-
        structure(
            list(
                content = resp_list$resp_200,
                uri = resp_list$uri,
                error_pages = resp_list$resp_error,
                df = df
            ),
            class = "yahoo_fantasy_api"
        )

    return(data_list)

})

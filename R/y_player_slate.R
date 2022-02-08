#' Get full slate of players from Yahoo! Fantasy API.
#'
#' Every game or league has a slate of players eligible to be on a team.  This function gets them for you.
#'
#' This function is not intended to get a subset of players, i.e. top 100 players.
#' Use `y_players()` for that case.
#'
#' Note: this function uses janitor::make_clean_names and as a result is a bit slow.
#'
#' @param key Game key or league key as a string in the form "000.l.0000".  These ids can be found with `y_games()` and `y_teams()`.
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param debug Print uri and page counts to console as functions runs.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble
#' @export
y_player_slate <- memoise::memoise(function(key = NULL, token_name = NULL, debug = FALSE, quiet = TRUE) {


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


    # Eligible key types.
    e_key_types <- c("games", "leagues")

    # Assign a resource to each key and count.
    # Function then selects most frequently occurring resource and assigns value to resource.
    c(resource, key, .) %<-% .multiple_resource_key_check(key, e_key_types = e_key_types)

    # Determine what game the player_key belongs to.
    game_key <- .game_key_assign_fn(key)

    if(!quiet){cat(crayon::cyan("game is", game_key), sep = " ")}

    # Subset out player_keys belonging to game_key.
    # Function can't call multiple game resources.
    key <-
        stringr::str_subset(string = key, pattern = game_key)


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    subresource <- "players"
    uri_out <- switch(resource, "games" = "game_keys", "leagues" = "league_keys")

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                FUNCTION DEFS                             ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Vectorize seq.
    # This function will be used to sequence pages that return errors
    seq2 <- Vectorize(seq.default, vectorize.args = c("from"))

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                              function - part 1                           ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # The Yahoo! api will return a max of 25 players per request and total number of
    # players in the full player slate is not known.  This loop will continue to get
    # the next 25 players as long as the the previous response contained 25 players.

    ##~~~~~~~~~~~~~~~~~~~~
    ##  ~ while loop  ----
    ##~~~~~~~~~~~~~~~~~~~~


    #...........................empty list...........................


    resp_list <- list(uri = NULL,
                      resp_200 = NULL,
                      resp_error = NULL)

    #......................while loop conditions.....................

    page_start <- 0
    count <- 25

    while (count == 25) {

        #..............................uri...............................

        uri <- glue::glue(
            "https://fantasysports.yahooapis.com/fantasy/v2/{resource};{uri_out}={key}/{subresource};start={page_start}?format=json"
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
                .y_parse_response(r, "fantasy_content") %>%
                purrr::map(1) %>%
                purrr::keep(purrr::is_list) %>%
                purrr::flatten()

            # assign response to list element with corresponding page start

            resp_list[["resp_200"]][[as.character(page_start)]] <- r_parsed


            #..........................update count..........................

            count <- purrr::pluck(r_parsed, 1, 2, 1, "count")


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

    cat(crayon::cyan("pages", error_pages, "returned an http error, retrying\n"))

    error_pages_seq <- c(seq2(from = error_pages, length.out = 25))

    uri <- glue::glue(
        "https://fantasysports.yahooapis.com/fantasy/v2/{resource};{uri_out}={key}/{subresource};start={error_pages_seq};count=1?format=json"
    )

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

    # Parse the "re-gotten" responses and append to resp_200
    resp_list[["resp_200"]] <-
        purrr::map(reget_response,
                   function(x) {
                       .y_parse_response(x, "fantasy_content") %>%
                           purrr::map(1) %>%
                           purrr::flatten()
                       }) %>%
        append(resp_list[["resp_200"]], .,
               after = length(resp_list[["resp_200"]]) + 1)


    #...............................df...............................

    if(!debug){


        cat(crayon::cyan("parsing", length(resp_list$resp_200), "responses...\n"))

        player_list <-
            resp_list$resp_200 %>%
            # Pluck 1 here because list returns a resource of "game" or "league" and I didn't want to
            # write a big long switchery-do when this function will only ever be operation on one
            # game or league resource at a time.
            purrr::map( ~purrr::pluck(.x, 1, 2, "players") %>% purrr::keep(purrr::is_list)) %>%
            purrr::flatten()

        pb <- progress::progress_bar$new(total = length(player_list))

        subresource_parse <- function(x){
            pb$tick()
            df <- .player_meta_parse_fn(x)
            return(df)
        }

        df <-
            purrr::map_df(player_list, subresource_parse)

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

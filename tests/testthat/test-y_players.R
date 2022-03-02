library(YFAR)

test_that("uri generates_properly", {

    y_players_uri_gen_fn <- function(key = NULL, start = 0, number_of_players = 100, ...){


        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##                                    CHECKS                                ----
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



        # Eligible key types.
        e_key_types <- c("games", "leagues")

        # Assign a resource to each key and count.
        # Function then selects most frequently occurring resource and assigns value to resource.
        c(resource, key, .) %<-% .multiple_resource_key_check(key, e_key_types = e_key_types)

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  ARGUMENTS                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # resource assigned in if statement above.
    subresource <- "players"
    uri_out <-
        switch(resource, "games" = "game_keys=", "leagues" = "league_keys=")

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

    # Page params
    # Again the API only returns 25 at a time so anything over 25 needs to be broken up.
    # Call internal function to sequence start and count numbers into params.
    # I.E. take count = 100, start = 0 into 4 uri's that call 25 players each.
    # https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/players;start=0;count=25?format=json
    page_params <-
        .seq_pages_fn(start = start, count = number_of_players, i = 25)

    # Passed by ... argument
    other_uri_params <- list(...)

    key_path <-
        .uri_path_packer(key)

    # Create paths using .uri_path_package(key) to pack player keys into groups of 25.
    uri_parsed$params <-
        stringr::str_c(uri_out, key_path, "/", subresource, ";", page_params)

    # Modify page params with other uri params supplied to ...
    # If other_uri_params is not empty they are supplied collapse with sep=";" and paste them to the end of pp.
    # Paste ... names to values to make up a param.
    # I.E. take list(sort=1) and turn it into ;sort=1.
    # This is essentially a work-around for glue returning and empty string when ... is empty.
    if (!purrr::is_empty(other_uri_params)) {
        pp <-
            stringr::str_flatten(stringr::str_c(names(other_uri_params), other_uri_params, sep = "="),
                                 collapse = ";")
        uri_parsed$params <-
            stringr::str_c(uri_parsed$params, pp, sep = ";")
    }

    # Build uris.
    uri <- httr::build_url(uri_parsed)

    return(uri)
}


    expect_equal(
        y_players_uri_gen_fn("411"),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=0;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=25;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=50;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=75;count=25?format=json")
        )

    expect_equal(
        y_players_uri_gen_fn("411", start = 0, number_of_players = 25),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=0;count=25?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn("411", start = 100),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=100;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=125;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=150;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=175;count=25?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn("411", start = 100, number_of_players = 200),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=100;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=125;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=150;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=175;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=200;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=225;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=250;count=25?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=275;count=25?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn("411", start = 100, number_of_players = 200, sort="AR"),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=100;count=25;sort=AR?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=125;count=25;sort=AR?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=150;count=25;sort=AR?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=175;count=25;sort=AR?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=200;count=25;sort=AR?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=225;count=25;sort=AR?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=250;count=25;sort=AR?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=275;count=25;sort=AR?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn("411", start = 100, sort = 1, status = "FA"),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=100;count=25;sort=1;status=FA?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=125;count=25;sort=1;status=FA?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=150;count=25;sort=1;status=FA?format=json",
          "https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=175;count=25;sort=1;status=FA?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn("411", start = 50, number_of_players = 25),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=50;count=25?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn("411.l.1239", start = 50, number_of_players = 25),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239/players;start=50;count=25?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn(c("411.l.1239", "411.l.1240"), start = 50, number_of_players = 25),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=50;count=25?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn(c("411.l.1239", "411.l.1240"), start = 50, number_of_players = 25, sort = 1, status = "FA", position = "C"),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=50;count=25;sort=1;status=FA;position=C?format=json")
    )

    # expect_equal(
    #     y_players_uri_gen_fn(c("411.l.1240", "411"), start = 50, number_of_players = 25, sort = 1, status = "FA", position = "C"),
    #     c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411/players;start=50;count=25;sort=1;status=FA;position=C?format=json")
    # )

    expect_equal(
        y_players_uri_gen_fn(c("411", "306", "411.l.1239"), start = 50, number_of_players = 25, sort = 1, status = "FA", position = "C"),
        c("https://fantasysports.yahooapis.com/fantasy/v2/games;game_keys=411,306/players;start=50;count=25;sort=1;status=FA;position=C?format=json")
    )

    expect_equal(
        y_players_uri_gen_fn(c("411.l.1239", "411.l.1240"), start = 100, number_of_players = 250, sort = 1, status = "FA", position = "C"),
        c("https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=100;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=125;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=150;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=175;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=200;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=225;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=250;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=275;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=300;count=25;sort=1;status=FA;position=C?format=json",
        "https://fantasysports.yahooapis.com/fantasy/v2/leagues;league_keys=411.l.1239,411.l.1240/players;start=325;count=25;sort=1;status=FA;position=C?format=json"
    )
    )

})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               GAMES RESOURCE                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("One game resource is parsed to a tibble",{

    # Declare desired mock_uri.
    mock_uri <- "https://fantasysports.yahooapis.com/fantasy/v2/games;411/players;s=50;c=25;s=1;s=FA;p=C?format=json"

    # Expected resource
    resource <-
        "games"

    # Get response.
    r <-
        with_mock_api({
            purrr::map(mock_uri, .y_get_response)
        })

    # Parsed response.
    r_parsed <-
        purrr::map(r, .y_parse_response, "fantasy_content", resource)

    # Test that response uri and declared uri are equal.
    expect_equal(mock_uri,
                 purrr::map_chr(r, purrr::pluck, "url"))

    # Test not null.
    expect_true(!is.null(r_parsed))

    # Test not empty.
    expect_true(!purrr::is_empty(r_parsed))

    # Define parse function relative to resource value.
    resource_parse_fn <-
        switch(resource,
               "games" = {
                   .game_resource_parse_fn
               },
               "leagues" = {
                   .league_resource_parse_fn
               })

    initial_pluck <-
        switch(resource,
               "games" = list("game", 2, 1),
               "leagues" = list("league", 2, 1)
        )

    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        list_pre_process_fn()

    df <-
        preprocess %>%
        purrr::map_df(resource_parse_fn, pluck_args = initial_pluck, fn = function(x) purrr::map_df(x, .player_resource_parse_fn))

    # Test that a tibble was returned from parsing.
    expect_true(tibble::is_tibble(df), TRUE)


    # Expected colnames.
    expected_colnames <-
        c("player_key", "player_id", "player_name_full", "player_name_first",
          "player_name_last", "player_name_ascii_first", "player_name_ascii_last",
          "player_editorial_player_key", "player_editorial_team_key", "player_editorial_team_full_name",
          "player_editorial_team_abbr", "player_uniform_number", "player_display_position",
          "player_headshot_url", "player_headshot_size", "player_image_url",
          "player_is_undroppable", "player_position_type", "player_eligible_positions_position",
          "player_has_player_notes", "player_notes_last_timestamp", "player_eligible_positions_position_2",
          "player_has_recent_player_notes", "player_eligible_positions_position_3",
          "player_status", "player_status_full", "player_injury_note",
          "game_key", "game_id", "game_name", "game_code", "game_type",
          "game_url", "game_season", "game_is_registration_over", "game_is_game_over",
          "game_is_offseason")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)

})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              LEAGUES RESOURCE                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Two leagues resources is parsed to a tibble",{

    # Declare desired mock uri.
    mock_uri <- "https://fantasysports.yahooapis.com/fantasy/v2/leagues;411.l.1239,411.l.1240/players;s=0;c=25;s=AR;s=ALL?format=json"

    # Expected resource
    resource <-
        "leagues"

    # Get response.
    r <-
        with_mock_api({
            purrr::map(mock_uri, .y_get_response)
        })

    # Parsed response.
    r_parsed <-
        purrr::map(r, .y_parse_response, "fantasy_content", resource)

    # Test that response uri and declared uri are equal.
    expect_equal(mock_uri,
                 purrr::map_chr(r, purrr::pluck, "url"))

    # Test not null.
    expect_true(!is.null(r_parsed))

    # Test not empty.
    expect_true(!purrr::is_empty(r_parsed))

    # Define parse function relative to resource value.
    # Define parse function relative to resource value.
    resource_parse_fn <-
        switch(resource,
               "games" = {
                   .game_resource_parse_fn
               },
               "leagues" = {
                   .league_resource_parse_fn
               })

    initial_pluck <-
        switch(resource,
               "games" = list("game", 2, 1),
               "leagues" = list("league", 2, 1)
        )

    preprocess <-
        r_parsed %>%
        purrr::flatten() %>%
        list_pre_process_fn()

    df <-
        preprocess %>%
        purrr::map_df(resource_parse_fn, pluck_args = initial_pluck, fn = function(x) purrr::map_df(x, .player_resource_parse_fn))

    # Test that a tibble was returned from parsing.
    expect_true(tibble::is_tibble(df), TRUE)


    # Expected colnames.
    expected_colnames <-
        c("league_key", "league_id", "league_name", "league_url", "league_logo_url",
          "league_draft_status", "league_num_teams", "league_edit_key",
          "league_weekly_deadline", "league_update_timestamp", "league_scoring_type",
          "league_type", "league_renew", "league_renewed", "league_iris_group_chat_id",
          "league_allow_add_to_dl_extra_pos", "league_is_pro_league", "league_is_cash_league",
          "league_current_week", "league_start_week", "league_start_date",
          "league_end_week", "league_end_date", "league_game_code", "league_season",
          "player_key", "player_id", "player_name_full", "player_name_first",
          "player_name_last", "player_name_ascii_first", "player_name_ascii_last",
          "player_editorial_player_key", "player_editorial_team_key", "player_editorial_team_full_name",
          "player_editorial_team_abbr", "player_uniform_number", "player_display_position",
          "player_headshot_url", "player_headshot_size", "player_image_url",
          "player_is_undroppable", "player_position_type", "player_primary_position",
          "player_eligible_positions_position", "player_has_player_notes",
          "player_notes_last_timestamp", "player_eligible_positions_position_2",
          "player_has_recent_player_notes", "player_status", "player_status_full",
          "player_injury_note", "player_on_disabled_list")

    # Test df colnames
    expect_named(df,
                 expected_colnames,
                 ignore.order = TRUE,
                 ignore.case = TRUE)

})

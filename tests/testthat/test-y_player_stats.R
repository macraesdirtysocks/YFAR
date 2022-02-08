library(YFAR)

test_that("uri generates properly", {
  y_player_stats_uri_generator <-
    function(player_key = NULL, game_date = NULL) {


      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                                  ARGUMENTS                               ----
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      resource <- "players"
      subresource <- "stats"
      uri_out <- "player_keys="

      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                                    CHECKS                                ----
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Check if keys are type league, remove FALSE and duplicates.
      key <- .single_resource_key_check(player_key, .player_key_check)

      # Determine what game the player_key belongs to.
      game_key <- .game_key_assign_fn(key)

      # Subset out player_keys belonging to game_key.
      # Function can't call multiple game resources.
      key <-
        stringr::str_subset(string = key, pattern = game_key)

      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                                     URI                                  ----
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

      key_path <-
        .uri_path_packer(key, 25)

      uri_parsed$params <-
        stringr::str_c(uri_out, key_path, "/", subresource, sep = "")

      # If game_date not null run .date_parm_fn which will check date for compatibility with
      # game key and assign date param.  This essentially makes sure nfl leagues get week= game date
      # parameter.  Else NULL.
      if(!is.null(game_date)){
        date_param <- .date_param_fn(game_key, game_date)
      } else{
        date_param <- NULL
      }

      # If date_param is not null concatenate to end of params.
      # If a game_date is supplied and compatible it will be pasted to the end of the uri params.
      if(!is.null(date_param)) {
        uri_parsed$params <-
          stringr::str_c(uri_parsed$params, date_param, sep = ";")
      }

      uri <-
        httr::build_url(uri_parsed)
    }


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                                  TEST URI                                ----
  ##                                                                            ~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



  expect_identical(
    y_player_stats_uri_generator("411.p.6369", game_date = "2022-01-29"),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6369/stats;type=date;date=2022-01-29?format=json"
  )

  expect_identical(
    y_player_stats_uri_generator("222.p.6369", 1),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=222.p.6369/stats;type=week;week=1?format=json"
  )

  expect_identical(
    y_player_stats_uri_generator("222.p.6369"),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=222.p.6369/stats?format=json"
  )

  expect_identical(
    y_player_stats_uri_generator(
      c(
        "411.p.6369",
        "411.p.6369",
        "411.p.6000",
        "411.p.6000",
        "411.p.5000"
      )
    ),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6369,411.p.6000,411.p.5000/stats?format=json"
  )

  expect_identical(
    y_player_stats_uri_generator(
      c(
        "411.p.6369",
        "411.p.6369",
        "411.p.6000",
        "411.p.6000",
        "411.p.5000"
      ),
      game_date = 1
    ),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6369,411.p.6000,411.p.5000/stats?format=json"
  )

  expect_identical(
    y_player_stats_uri_generator(
      c(
        "411.p.6369",
        "411.p.6369",
        "411.p.6000",
        "411.p.6000",
        "411.p.5000"
      ),
      game_date = c(1, 2, 3)
    ),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6369,411.p.6000,411.p.5000/stats?format=json"
  )

  expect_identical(
    y_player_stats_uri_generator(
      c(
        "411.p.6369",
        "411.p.6369",
        "411.p.6000",
        "411.p.6000",
        "411.p.5000"
      ),
      game_date = c(1, 2, "2022-01-01")
    ),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6369,411.p.6000,411.p.5000/stats;type=date;date=2022-01-01?format=json"
  )

  expect_identical(
      y_player_stats_uri_generator(
          c(
              "406.p.6369",
              "406.p.6369",
              "406.p.6000",
              "411.p.6000",
              "411.p.5000",
              "411.p.4000"
          ),
          game_date = 1
      ),
      "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6000,411.p.5000,411.p.4000/stats?format=json"
  )

  # expect_identical(
  #     y_player_stats_uri_generator(
  #         c(
  #             "406.p.6369",
  #             "406.p.6369",
  #             "406.p.6000",
  #             "411.p.6000",
  #             "411.p.5000"
  #         ),
  #         game_date = c("2022-01-01", "2022-02-01", 1)
  #     ),
  #     "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=406.p.6369,406.p.6000/stats;type=week;week=1?format=json"
  # )

  expect_identical(
    y_player_stats_uri_generator(
      c(
        "411.p.6369",
        "411.p.6369",
        "411.p.6000",
        "411.p.6000",
        "411.p.5000"
      ),
      game_date = c("2022-01-01", "2022-02-01", 1)
    ),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6369,411.p.6000,411.p.5000/stats;type=date;date=2022-01-01,2022-02-01?format=json"
  )

  expect_identical(
    y_player_stats_uri_generator(
      c(
        "411.p.6369",
        "411.p.6369",
        "411.p.6000",
        "411.p.6000",
        "411.p.5000",
        "411.p.1234",
        "406.p.6000"
      ),
      game_date = c("2022-01-01", "2022-02-01", 1, 2021 - 12 - 01)
    ),
    "https://fantasysports.yahooapis.com/fantasy/v2/players;player_keys=411.p.6369,411.p.6000,411.p.5000,411.p.1234/stats;type=date;date=2022-01-01,2022-02-01?format=json"
  )

})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                SINGLE PLAYER                             ----
##                                                                            ~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


testthat::test_that("Single plaeyr response for a is valid and parsed to a tibble.", {
  # Declare desired uri.
  mock_uri <-
    "https://fantasysports.yahooapis.com/fantasy/v2/players;411.p.6369/stats?format=json"

  # Expected resource
  resource <-
    "players"

  # Get response.
  r <-
    with_mock_api({
      purrr::map(mock_uri, .y_get_response)
    })

  # Parsed response.
  r_parsed <-
    purrr::map(r, .y_parse_response, "fantasy_content", resource)

  # Test that response uri and declared uri are equal.
  expect_equal(
    mock_uri,
    purrr::map_chr(r, purrr::pluck, "url")
  )

  # Test not null.
  expect_true(!is.null(r_parsed))

  # Test not empty.
  expect_true(!purrr::is_empty(r_parsed))

  # Preprocess list.  This step is verbatim from y_matchups.
  preprocess <-
    r_parsed %>%
    purrr::flatten() %>%
    purrr::keep(purrr::is_list)

  # Parse response to tibble.
  df <-
    preprocess %>%
    purrr::map_df(.player_resource_parse_fn, .player_stats_parse)

  # Test that a tibble was returned from parsing.
  expect_true(tibble::is_tibble(df), TRUE)

  # Expected colnames.
  expected_colnames <-
    c(
      "player_key",
      "player_id",
      "player_full",
      "player_first",
      "player_last",
      "player_ascii_first",
      "player_ascii_last",
      "player_editorial_player_key",
      "player_editorial_team_key",
      "player_editorial_team_full_name",
      "player_editorial_team_abbr",
      "player_uniform_number",
      "player_display_position",
      "player_url",
      "player_size",
      "player_image_url",
      "player_is_undroppable",
      "player_position_type",
      "player_position",
      "player_position_2",
      "player_has_player_notes",
      "player_notes_last_timestamp",
      "coverage_type",
      "season",
      "gp",
      "gp_2",
      "g",
      "a",
      "p",
      "x",
      "pim",
      "ppg",
      "ppa",
      "ppp",
      "shg",
      "sha",
      "shp",
      "gwg",
      "gtg",
      "sog",
      "sh_percent",
      "fw",
      "fl",
      "hit",
      "blk"
    )

  # Test df colnames
  expect_named(df,
    expected_colnames,
    ignore.order = TRUE,
    ignore.case = TRUE
  )
})



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            SINGLE PLAYER W DATE                          ----
##                                                                            ~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

testthat::test_that("Single player with date response for a is valid and parsed to a tibble.", {
  # Declare desired uri.
  mock_uri <-
    "https://fantasysports.yahooapis.com/fantasy/v2/players;411.p.6369/stats;type=date;date=2021-12-01?format=json"

  # Expected resource
  resource <-
    "players"

  # Get response.
  r <-
    with_mock_api({
      purrr::map(mock_uri, .y_get_response)
    })

  # Parsed response.
  r_parsed <-
    purrr::map(r, .y_parse_response, "fantasy_content", resource)

  # Test that response uri and declared uri are equal.
  expect_equal(
    mock_uri,
    purrr::map_chr(r, purrr::pluck, "url")
  )

  # Test not null.
  expect_true(!is.null(r_parsed))

  # Test not empty.
  expect_true(!purrr::is_empty(r_parsed))

  # Preprocess list.  This step is verbatim from y_matchups.
  preprocess <-
    r_parsed %>%
    purrr::flatten() %>%
    purrr::keep(purrr::is_list)

  # Parse response to tibble.
  df <-
    preprocess %>%
    purrr::map_df(.player_resource_parse_fn, .player_stats_parse)

  # Test that a tibble was returned from parsing.
  expect_true(tibble::is_tibble(df), TRUE)

  # Expected colnames.
  expected_colnames <-
    c(
      "player_key",
      "player_id",
      "player_full",
      "player_first",
      "player_last",
      "player_ascii_first",
      "player_ascii_last",
      "player_editorial_player_key",
      "player_editorial_team_key",
      "player_editorial_team_full_name",
      "player_editorial_team_abbr",
      "player_uniform_number",
      "player_display_position",
      "player_url",
      "player_size",
      "player_image_url",
      "player_is_undroppable",
      "player_position_type",
      "player_position",
      "player_position_2",
      "player_has_player_notes",
      "player_notes_last_timestamp",
      "coverage_type",
      "date",
      "gp",
      "gp_2",
      "g",
      "a",
      "p",
      "x",
      "pim",
      "ppg",
      "ppa",
      "ppp",
      "shg",
      "sha",
      "shp",
      "gwg",
      "gtg",
      "sog",
      "sh_percent",
      "fw",
      "fl",
      "hit",
      "blk"
    )

  # Test df colnames
  expect_named(df,
    expected_colnames,
    ignore.order = TRUE,
    ignore.case = TRUE
  )
})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                TWO PLAYERS                               ----
##                                                                            ~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

testthat::test_that("Two player response for a is valid and parsed to a tibble.", {
  # Declare desired uri.
  mock_uri <-
    "https://fantasysports.yahooapis.com/fantasy/v2/players;411.p.6369,411.p.6391/stats?format=json"

  # Expected resource
  resource <-
    "players"

  # Get response.
  r <-
    with_mock_api({
      purrr::map(mock_uri, .y_get_response)
    })

  # Parsed response.
  r_parsed <-
    purrr::map(r, .y_parse_response, "fantasy_content", resource)

  # Test that response uri and declared uri are equal.
  expect_equal(
    mock_uri,
    purrr::map_chr(r, purrr::pluck, "url")
  )

  # Test not null.
  expect_true(!is.null(r_parsed))

  # Test not empty.
  expect_true(!purrr::is_empty(r_parsed))

  # Preprocess list.  This step is verbatim from y_matchups.
  preprocess <-
    r_parsed %>%
    purrr::flatten() %>%
    purrr::keep(purrr::is_list)

  # Parse response to tibble.
  df <-
    preprocess %>%
    purrr::map_df(.player_resource_parse_fn, .player_stats_parse)

  # Test that a tibble was returned from parsing.
  expect_true(tibble::is_tibble(df), TRUE)

  # Expected colnames.
  expected_colnames <-
    c(
      "player_key", "player_id", "player_full", "player_first", "player_last",
      "player_ascii_first", "player_ascii_last", "player_editorial_player_key",
      "player_editorial_team_key", "player_editorial_team_full_name",
      "player_editorial_team_abbr", "player_uniform_number", "player_display_position",
      "player_url", "player_size", "player_image_url", "player_is_undroppable",
      "player_position_type", "player_position", "player_position_2",
      "player_has_player_notes", "player_notes_last_timestamp", "coverage_type",
      "season", "gp", "gp_2", "g", "a", "p", "x", "pim", "ppg", "ppa",
      "ppp", "shg", "sha", "shp", "gwg", "gtg", "sog", "sh_percent",
      "fw", "fl", "hit", "blk"
    )

  # Test df colnames
  expect_named(df,
    expected_colnames,
    ignore.order = TRUE,
    ignore.case = TRUE
  )
})

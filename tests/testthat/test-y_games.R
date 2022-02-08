library(YFAR)

testthat::test_that("uri is generated properly", {

    y_games_uri_gen_test_fn <- function()
        {

        resource <- "users"
        subresource <- "games"
        collection <- "leagues"

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
        }

    # Test games uri is built properly.
        expect_identical(
            y_games_uri_gen_test_fn(),
            "https://fantasysports.yahooapis.com/fantasy/v2/users;use_login=1/games/leagues?format=json")
    })

with_mock_api({

    test_that("Response parses correctly.",{

    resource <- "users"
    uri <- "https://fantasysports.yahooapis.com/fantasy/v2/users;use_login=1/games/leagues?format=json"
    r <- .y_get_response(uri = uri)

    # Test that response uri and uri are equal.
    test_that("Response uri is equal to input uri.",{
        expect_equal(
            uri,
            r[["url"]])
    })

    # Test that the user element contains a games element
    test_that("user element contains a games element",{
        expect_identical(
        names(.y_parse_response(r, "fantasy_content", "users", "0", "user", 2)),
        "games")
        })

    r_parsed <-
        .y_parse_response(r, "fantasy_content", resource)

    preprocess <-
        r_parsed %>%
        purrr::pluck("0", "user", 2, "games") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact()

    # Subset out special tournaments and promotional leagues i.e. free NFL money leagues and golf.
    preprocess <- preprocess[purrr::map_depth(preprocess, 2, purrr::every, purrr::is_list) %>% purrr::map_lgl(1)]

    # Test that all elements of r_parsed are named leagues
    test_that("All game elements contain a leagues list", {
        # Pluck out games resource and then test that all elements are league elements.
        games_resource_names <-
            purrr::map(preprocess, purrr::pluck, "game", 2) %>%
            purrr::compact()

        expect_true(vctrs::vec_equal(
            purrr::map_chr(games_resource_names, names) %>% vctrs::vec_unique(),
            c("0" = "leagues")))
    })

    df <-
        purrr::map_df(preprocess, .game_resource_parse_fn, .league_meta_parse_fn)

    # Test that df is a tibble
    test_that("A tibble is returned after parsing",{
        expect_true(tibble::is_tibble(df))
        expect_length(df, 38)
    })

    # Expected colnames
    expected_df_colnames <-
        c("game_key", "game_id", "game_name", "game_code", "game_type",
          "game_url", "game_season", "game_is_registration_over", "game_is_game_over",
          "game_is_offseason", "league_key", "league_id", "league_name",
          "league_url", "league_logo_url", "league_password", "league_draft_status",
          "league_num_teams", "league_edit_key", "league_weekly_deadline",
          "league_update_timestamp", "league_scoring_type", "league_type",
          "league_renew", "league_renewed", "league_iris_group_chat_id",
          "league_short_invitation_url", "league_allow_add_to_dl_extra_pos",
          "league_is_pro_league", "league_is_cash_league", "league_current_week",
          "league_start_week", "league_start_date", "league_end_week",
          "league_end_date", "league_is_finished", "league_game_code",
          "league_season")

    # Test colnames sare s expected.
    expect_named(df, expected_df_colnames, ignore.order = TRUE, ignore.case = TRUE)

})
})

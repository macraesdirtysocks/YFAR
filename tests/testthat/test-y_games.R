context("Get game data")
library(YFAR)

with_mock_api({

testthat::test_that("request returns valid response and is parsed to a tibble",{

    uri <- "https://fantasysports.yahooapis.com/fantasy/v2/users;use_login=1/games/leagues?format=json"
    r <- .y_get_response(uri = uri)
    testthat::expect_identical(r$url, uri)
    testthat::expect_s3_class(r, class = "response")
    testthat::expect_identical(httr::http_type(r), "application/json")
    testthat::expect_false(httr::http_error(r))
    testthat::expect_identical(
        names(.y_parse_response(r, "fantasy_content", "users", "0", "user", 2)), "games")

    r_parsed <- .y_parse_response(r, "fantasy_content", "users", "0", "user", 2, "games")

    testthat::expect_gte(length(r_parsed), 1)

    preprocess <-
        r_parsed %>%
        purrr::map(purrr::pluck, "game") %>%
        purrr::keep(purrr::is_list) %>%
        purrr::compact() %>%
        purrr::keep(purrr::every, purrr::is_list)

    df <- purrr::map_df(preprocess, .y_games_parse)

    testthat::expect_equal(tibble::is_tibble(df), TRUE)

    x <- c("game_key", "game_id", "game_name", "game_code", "game_type",
           "game_url", "game_season", "game_is_registration_over", "is_game_over",
           "game_is_offseason", "league_key", "league_id", "league_name",
           "league_url", "league_logo_url", "league_password", "league_draft_status",
           "league_num_teams", "league_edit_key", "league_weekly_deadline",
           "league_update_timestamp", "league_scoring_type", "league_type",
           "league_renew", "league_renewed", "league_iris_group_chat_id",
           "league_short_invitation_url", "league_allow_add_to_dl_extra_pos",
           "is_pro_league", "is_cash_league", "league_current_week", "league_start_week",
           "league_start_date", "league_end_week", "league_end_date", "league_is_finished",
           "league_game_code", "league_season")

    testthat::expect_named(df, x, ignore.order = TRUE, ignore.case = TRUE)

})
})

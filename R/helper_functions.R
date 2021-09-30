########################################################################################################################
########################################################################################################################

# Check for token class in .GLobalEnv

#' @export
token_check <- function(){
    purrr::map(.x = ls(name = .GlobalEnv), .f = get) %>%
        purrr::map_chr(.f = janitor::describe_class) %>%
        stringr::str_detect(pattern = "Token") %>%
        sum()
}

########################################################################################################################
########################################################################################################################

# Get response object from Yahoo! API

#' @export
y_get_response <- function(x, y) {
    httr::GET(
        url = x,
        httr::add_headers(
                  Authorization = stringr::str_c(
                      "Bearer",
                      y$credentials$access_token, sep = " "
                      )
                  )
        )
}

########################################################################################################################
########################################################################################################################

# Parse response object from Yahoo! API

#' @export
y_parse_response <- function(x){
    jsonlite::fromJSON(
        httr::content(x, as = "text", encoding = "utf-8"), simplifyVector = FALSE) %>%
        purrr::pluck("fantasy_content", "league") %>%
        purrr::keep(purrr::is_list)
}

########################################################################################################################
########################################################################################################################

# Parse settings from a Yahoo! Fantasy Category League.

#' @export
category_league_settings <- function(r_parsed){

    league_meta <-
        r_parsed %>%
        purrr::pluck(1) %>%
        dplyr::bind_rows() %>%
        dplyr::add_column(info = "league_meta", .before = 1) %>%
        tidyr::nest(data = !info)

    league_settings <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1) %>%
        purrr::keep(purrr::negate(purrr::is_list)) %>%
        dplyr::bind_rows() %>%
        dplyr::add_column(info = "league_settings", .before = 1) %>%
        tidyr::nest(data = !info)

    roster_positions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "roster_positions") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(., ~dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = everything(), as.character))) %>%
        dplyr::add_column(info = "roster_positions", .before = 1) %>%
        tidyr::nest(data = !info)

    stat_categories <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "stat_categories", "stats") %>%
        purrr::map(purrr::flatten) %>%
        purrr::transpose() %>%
        purrr::map_at("stat_position_types", purrr::map, purrr::pluck, 1, "stat_position_type", "position_type") %>%
        purrr::transpose() %>%
        purrr::map_df(dplyr::bind_rows) %>%
        dplyr::add_column(info = "stat_categories", .before = 1) %>%
        tidyr::nest(data = !info)

    divisions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "divisions") %>%
        purrr::map(purrr::pluck, "division") %>%
        purrr::map_df(dplyr::bind_rows) %>%
        dplyr::add_column(info = "divisons", .before = 1) %>%
        tidyr::nest(data = !info)

    df <- dplyr::bind_rows(league_meta, league_settings, roster_positions, stat_categories, divisions)

    return(df)
}

########################################################################################################################
########################################################################################################################

# Parse settings from a Yahoo! Fantasy Point League.

#' @export
point_league_settings <- function(r_parsed) {

    league_meta <-
        r_parsed %>%
        purrr::pluck(1) %>%
        dplyr::bind_rows() %>%
        dplyr::add_column(info = "league_meta", .before = 1) %>%
        tidyr::nest(data = !info)

    league_settings <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1) %>%
        purrr::keep(purrr::negate(purrr::is_list)) %>%
        dplyr::bind_rows() %>%
        dplyr::add_column(info = "league_settings", .before = 1) %>%
        tidyr::nest(data = !info)

    roster_positions <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "roster_positions") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(., ~dplyr::bind_rows(.) %>%  dplyr::mutate(dplyr::across(.cols = everything(), as.character))) %>%
        dplyr::add_column(info = "roster_positions", .before = 1) %>%
        tidyr::nest(data = !info)

    stat_categories <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "stat_categories", "stats") %>%
        purrr::map(purrr::flatten) %>%
        purrr::transpose() %>%
        purrr::map_at("stat_position_types", purrr::map, purrr::pluck, 1, "stat_position_type", "position_type") %>%
        purrr::transpose() %>%
        purrr::map_df(dplyr::bind_rows)

    stat_modifiers <-
        r_parsed %>%
        purrr::pluck(2, "settings", 1, "stat_modifiers", "stats") %>%
        purrr::map(purrr::flatten) %>%
        purrr::map_df(dplyr::bind_rows)

    stats <-
        dplyr::left_join(stat_categories, stat_modifiers, by = "stat_id") %>%
        dplyr::add_column(info = "stat_modifiers", .before = 1) %>%
        dplyr::rename(modifier_value = value) %>%
        tidyr::nest(data = !info)

    df <- dplyr::bind_rows(league_meta, league_settings, roster_positions, stats)

    return(df)
}

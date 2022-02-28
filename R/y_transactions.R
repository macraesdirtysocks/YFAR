#' Get transaction data from Yahoo! Fantasy API.
#'
#' This function returns data from the transactions resource.
#'   - Transactions include:
#'     - add
#'     - drop
#'     - trade
#'     - commissioner
#'   A single transaction can have multiple parts such as an add drop.
#'   Waivers transactions are not included in this function.
#'
#' @param key league key or team key as a string in the form "000.l.0000" or "000.l.0000.t.0".
#' @param token_name Name used for assignment when creating token object with `y_create_token()`.
#' @param count Number of transactions to return.
#' @param transaction_type Filter argument for request.  Accepts 1 of "commish", "add", "drop", "trade".
#' @param debug Returns a list of data such as uri call and content.  Useful for debugging.
#' @param quiet Print function activity.
#'
#' @return A tibble.
#' @export
y_transactions <- function(key = NULL, token_name = NULL, count = NULL, transaction_type = NULL, debug = FALSE, quiet = TRUE) {


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    TOKEN                                 ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Standardize token name
  api_token <- token_name
  .token_check(token_name, api_token, name = .GlobalEnv)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    CHECKS                                ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Eligible key types.
  e_key_types <- c("leagues", "teams")

  # Assign a resource to each key and count.
  # Function then selects most frequently occurring resource and assigns value to resource.
  c(resource, key, .) %<-% .multiple_resource_key_check(key, e_key_types = e_key_types)

  # quiet
  if (!quiet) {
    cat(crayon::cyan("Resource is", resource, "\n"), sep = "")
  }
  if (!quiet) {
    cat(crayon::cyan("Keys are...\n", stringr::str_flatten(key, collapse = "\n")), sep = "\n")
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                  ARGUMENTS                               ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # resource assigned in if statement above.
  subresource <- "transactions"
  uri_out <- switch(resource,
    "leagues" = "league_keys=",
    "teams" = "team_keys="
  )

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

  key_paths <-
    .uri_path_packer(key, 25)

  uri_parsed$params <-
    stringr::str_c(uri_out, key_paths, "/", subresource, sep = "")

  if (!is.null(count)) {
    # Sequence page numbers getting 25 each time.
    count_param_sequenced <- .seq_pages_fn(start = 0, count = count, i = 25)
    # count_param <- stringr::str_c(count_param_sequenced, sep = "=")
    uri_parsed$params <- stringr::str_c(uri_parsed$params, count_param_sequenced, sep = ";")
  }

  if (vctrs::vec_size(transaction_type) == 1) {
    # Single transaction type (type =)
    transaction_type_param <- stringr::str_c("type", transaction_type, sep = "=")
    uri_parsed$params <- stringr::str_c(uri_parsed$params, transaction_type_param, sep = ";")
  } else if (vctrs::vec_size(transaction_type) > 1) {
    # Multiple transaction types (types =)
    transaction_type_collapsed <- stringr::str_flatten(transaction_type, collapse = ",")
    transaction_type_param <- stringr::str_c("types", transaction_type_collapsed, sep = "=")
    uri_parsed$params <- stringr::str_c(uri_parsed$params, transaction_type_param, sep = ";")
  }

  uri <- httr::build_url(uri_parsed)

  if (!quiet) {
    cat(crayon::cyan("uri is \n", uri), sep = "")
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                GET RESPONSE                              ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  r <-
    purrr::map(uri, .y_get_response, api_token)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                          CHECK RESPONSE FOR ERRORS                       ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  if (sum(!purrr::map_lgl(r, httr::http_error)) <= 0) {
    stop(message(crayon::cyan("All requests returned errors. You may need a token refresh.")), call. = FALSE)
  }

  r <- r[!purrr::map_lgl(r, httr::http_error)]

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                   CONTENT                                ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  r_parsed <-
    purrr::map(r, .y_parse_response, "fantasy_content", resource)


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                PARSE CONTENT                             ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                      DF                                  ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  if (!debug) {

    # Resource parse function dependent on resource.
    resource_parse_fn <-
      switch(resource,
        "leagues" = {
          .league_resource_parse_fn
        },
        "teams" = {
          .team_resource_parse_fn
        }
      )

    # Initial pluck dependent on resource,
    initial_pluck <-
      switch(resource,
        "leagues" = list("league", 2, 1),
        "teams" = list("team", 2, 1)
      )


    # Preprocess parsed content.
    preprocess <-
      r_parsed %>%
      purrr::flatten() %>%
      purrr::map(list_pre_process_fn)

    # df
    df <-
      tryCatch(
        expr =
          preprocess %>%
          # Parse inital resource league or team.
          purrr::map_df(
            resource_parse_fn,
            pluck_args = initial_pluck,
              # Parse transaction resource,
            fn = function(x)purrr::map_df(
              x,
              .transaction_parse_fn,
              pluck_args = list("transaction", 2, 1),
                # Parse player resource.
                fn = function(x)
                  purrr::map_df(
                    x,
                    .player_resource_parse_fn,
                    pluck_args = list("player", 2),
                    fn = function(x) purrr::map_df(x, purrr::flatten_df)
                  )
              )
          ) %>%
          # Mutate time stamp from seconds to datetime.
          dplyr::mutate(
            transaction_timestamp = as.numeric(transaction_timestamp) %>%
              as.POSIXct(origin = "1970-01-01")
          ),
        error = function(e) {
          message(
            crayon::cyan(
              "Function failed while parsing games resource with .transaction_parse_fn. Returning debug list."
            )
          )
        }
      )

    if (tibble::is_tibble(df)) {return(df)
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                DEBUG RETURN                              ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  data_list <-
    structure(
      list(
        uri = uri,
        resource = resource,
        response = r,
        r_parsed = r_parsed
      ),
      class = "yahoo_fantasy_api"
    )

  return(data_list)
}

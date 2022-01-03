#' Create an Oauth2.0 token
#'
#' Creates an Oauth2.0 token object used to access the Yahoo Fantasy API
#' See httr::Token2.0 for details and functionality.
#'
#' The first time `y_create_token()` is run the console will ask for a code and a web browser will open to display the code.
#' Copy and paste this code into the console to create your access token.
#' If this step is being a nuisance and your default browser is Chrome try Firefox.
#'
#' By default caching is enabled and httr will create a .httr-oauth file and you can refresh your token with `my_token$refresh()`.
#'
#' @param my_key String provided my registering an application with Yahoo Developer Network
#' @param my_secret String provided my registering an application with Yahoo Developer Network
#' @param app_name String name of app, not necessarily what you provided to Yahoo Developer Network
#' @param ... Arguments passed on to httr::oauth2.0_token()
#'
#' @return Token2.0 object.
#' @export
y_create_token <- function(my_key = NULL, my_secret = NULL, app_name = NULL, ...) {


    # Check if a token exists in the global environment
    if (.token_count(name = .GlobalEnv) >= 1){

        warning(message("Token object already exists in the global environment.
        Use token$refresh()"))
    }

    stopifnot(!is.null(my_key) && is.character(my_key))
    stopifnot(!is.null(my_secret) && is.character(my_secret))
    stopifnot(!is.null(app_name) && is.character(app_name))

    #Create App
    myapp <-
        httr::oauth_app(appname = app_name,
                        key = my_key,
                        secret = my_secret)

    # Get token endpoints and create token
    token <-
        httr::oauth2.0_token(
            httr::oauth_endpoints("yahoo"),
            myapp,
            use_oob = TRUE,
            oob_value = "oob",
            ...)

    return(token)

}

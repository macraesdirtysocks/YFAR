#' Create an Oauth2.0 token
#'
#' Creates an Oauth2.0 token object used to access the Yahoo Fantasy API
#' See httr::Token2.0 for details.
#'
#' @param my_key a string provided my registering an application with Yahoo Developer Network
#' @param my_secret a string provided my registering an application with Yahoo Developer Network
#' @param app_name a string name of app, not necessarily what you provided to Yahoo Developer Network
#'
#' @return Token2.0 object.
#' @export

y_create_token <-function(my_key = NULL, my_secret = NULL, app_name = NULL) {

    stopifnot(!is.null(my_key) && is.character(my_key))
    stopifnot(!is.null(my_secret) && is.character(my_secret))
    stopifnot(!is.null(app_name) && is.character(app_name))

    #Create App
    myapp <- httr::oauth_app(appname = app_name,
                             key = my_key,
                             secret = my_secret)

    # Get token endpoints
    token <-
        httr::oauth2.0_token(httr::oauth_endpoints("yahoo"),
                             myapp,
                             use_oob = TRUE,
                             oob_value = "oob")

    return(token)

}

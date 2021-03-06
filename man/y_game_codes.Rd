% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/y_game_codes.R
\name{y_game_codes}
\alias{y_game_codes}
\title{Get game meta data from Yahoo! Fantasy API}
\usage{
y_game_codes(sport = NULL, token_name = NULL, debug = FALSE, quiet = TRUE)
}
\arguments{
\item{sport}{Fantasy sport as a lowercase string.  Accepted sports are "mlb", "nba", "nhl", "nfl".}

\item{token_name}{Name used for assignment when creating token object with y_create_token().}

\item{debug}{Returns a list of data such as uri call and content.  Useful for debugging.}

\item{quiet}{Print function activity.}
}
\value{
Tibble
}
\description{
Returns meta data from the GAME resource of the Yahoo API.
}
\examples{
# Not run
# y_game_codes(c("nfl", "mlb"), my_token)
}

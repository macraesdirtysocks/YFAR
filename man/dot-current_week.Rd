% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.current_week}
\alias{.current_week}
\title{Get current fantasy week}
\usage{
.current_week(game_id = NULL, token_name = NULL)
}
\arguments{
\item{game_id}{3 digit number which prefixes league_id.
Identifies Yahoo Fanatasy Sport i.e. 411 is 2021  fantasy hockey.
Can be found with `y_games()`.}

\item{token_name}{api token value assigned by `y_create_token()`}
}
\description{
Get current fantasy week
}
\keyword{internal}

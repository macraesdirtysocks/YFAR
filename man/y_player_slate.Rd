% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/y_player_slate.R
\name{y_player_slate}
\alias{y_player_slate}
\title{Get full slate of players from Yahoo! Fantasy API.}
\usage{
y_player_slate(key = NULL, token_name = NULL, debug = FALSE, quiet = TRUE)
}
\arguments{
\item{key}{Game key or league key as a string in the form "000.l.0000".  These ids can be found with `y_games()` and `y_teams()`.}

\item{token_name}{Name used for assignment when creating token object with `y_create_token()`.}

\item{debug}{Print uri and page counts to console as functions runs.  Useful for debugging.}

\item{quiet}{Print function activity.}
}
\value{
A tibble
}
\description{
Every game or league has a slate of players eligible to be on a team.  This function gets them for you.
}
\details{
This function is not intended to get a subset of players, i.e. top 100 players.
Use `y_players()` for that case.

Note: this function uses janitor::make_clean_names and as a result is a bit slow.
}

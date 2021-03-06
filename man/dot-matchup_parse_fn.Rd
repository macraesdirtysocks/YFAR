% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{.matchup_parse_fn}
\alias{.matchup_parse_fn}
\title{Parse match-up sub-resource}
\usage{
.matchup_parse_fn(x)
}
\arguments{
\item{x}{List containing a matchup element.}
}
\value{
A tibble
}
\description{
This function parses the match-up sub-resource.
}
\details{
Used to parsed a list with a "match-up" element.

Right now as a default it calls .yahoo_hockey_stat_categories() which converts stat_id numbers into
more readable word abbreviation categories i.e. converts 1 into g.
}
\keyword{internal}

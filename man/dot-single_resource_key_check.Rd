% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.single_resource_key_check}
\alias{.single_resource_key_check}
\title{Check a single key for validity}
\usage{
.single_resource_key_check(key, key_check_fn)
}
\arguments{
\item{key}{Vector of keys}

\item{key_check_fn}{One of internal .key_check functions.}
}
\value{
A Vector
}
\description{
This function checks the provided keys for uniformity.  If more that one type of key is detected
the function will select the key type appearing most often and removes the rest.
}
\details{
This function assigns resource variable.
This function alters key argument.
This function calls `.key_resource_assign()`
}
\keyword{internal}

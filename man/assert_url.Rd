% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert_url.R
\name{assert_url}
\alias{assert_url}
\title{Assert Existence of URL}
\source{
Code adapted from
\href{https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r}{Stack Overflow},
accessed on 2023-12-11 and authored by Bob Rudis.
}
\usage{
assert_url(url, ...)
}
\arguments{
\item{url}{'character' string.
URL}

\item{...}{Other arguments passed to the \code{\link[httr:HEAD]{httr::HEAD}} and \code{\link[httr:GET]{httr::GET}} functions.}
}
\value{
Returns \code{url} invisibly.
A \code{NULL} value is returned if the assertion fails for any reason.
}
\description{
Assert that a Uniform Resource Locator (URL) is complete and valid.
Requires that the \pkg{httr} package is available.
}
\examples{
\donttest{
assert_url("https://www.usgs.gov/")
}
}
\keyword{internal}

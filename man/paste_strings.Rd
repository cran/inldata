% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{paste_strings}
\alias{paste_strings}
\title{Concatenate Character Vectors}
\usage{
paste_strings(..., collapse = " ", recycle0 = FALSE)
}
\arguments{
\item{...}{'character' vectors (or objects coercible to character vectors).
Corresponding elements are to be concatenated.}

\item{collapse}{'character' string.
Seperates the results.}

\item{recycle0}{'logical' flag.
Whether a zero-length character argument should lead to a
zero-length character (\code{character(0)}) being returned.}
}
\value{
Returns a character vector.
}
\description{
Concatenate character vectors and omit empty strings.
}
\examples{
paste_strings(letters, c(), c(1, 2, 3))

paste_strings(letters, c(), c(1, 2, 3), recycle0 = TRUE)
}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}
\keyword{internal}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{write_lines}
\alias{write_lines}
\title{Write Lines to a File}
\usage{
write_lines(text, path, gz = FALSE)
}
\arguments{
\item{text}{'character' vector.
Text to write to file.}

\item{path}{'character' string.
Path to the file.}

\item{gz}{'logical' flag.
Whether to compress the file using Gzip.
The \code{.gz} extension is added to the file \code{path}.}
}
\value{
Invisibly returns the extracted path(s).
}
\description{
Write text lines to a file.
}
\examples{
path <- write_lines(
  text = "Test",
  path = tempfile(fileext = ".txt"),
  gz = TRUE
)

unlink(path)
}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}
\keyword{internal}

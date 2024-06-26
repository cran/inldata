% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_data_release.R
\name{make_data_release}
\alias{make_data_release}
\title{Create Data Release}
\usage{
make_data_release(
  metadata,
  package,
  destdir = getwd(),
  ...,
  bounding = NULL,
  rngdates = NULL,
  validate = FALSE
)
}
\arguments{
\item{metadata}{'character' string or named 'list'.
Either the path to a JSON formatted metadata file that contains general information
for the USGS data release (see \emph{Examples} section),
or a named list with the equivalent information.}

\item{package}{'character' string.
Name of a package.}

\item{destdir}{'character' string.
Destination directory to write files, with tilde-expansion performed.}

\item{...}{Additional arguments to be passed to the \code{\link{write_datasets}} function.
The \code{formats} argument, which is specified within the function, is the exception.}

\item{bounding}{'bbox', 'sf', 'SpatRaster', or 'PackedSpatRaster' spatial feature.
Object to compute spatial bounding coordinates from, see \code{\link[sf:st_bbox]{sf::st_bbox}} function.}

\item{rngdates}{'Date' or 'POSIXct' vector.
Object to compute the date range of observations from.}

\item{validate}{'logical' flag.
Whether to perform a metadata validation and stop execution if errors are found.
See \code{\link{validate_metadata}} function for system requirements.}
}
\value{
Named list of metadata created for spatial and non-spatial datasets.
}
\description{
Create U.S. Geological Survey (USGS) data release product
from R-package datasets and their documentation.
Requires that the \pkg{xml2} and \pkg{jsonlite} packages are available.
}
\details{
Citation entries for the \R package (software release) and accompanying data release
should be included in the package CITATION file, and documented in that order.
}
\examples{
destdir <- tempfile("")
rngdates <- c(samples$sample_dt, gwl$lev_dt) |> range()
l <- make_data_release(
  metadata = system.file("extdata/metadata.json", package = "inldata"),
  package = "inldata",
  destdir = destdir,
  include = "crs",
  quiet = TRUE,
  bounding = sites,
  rngdates = rngdates
)
str(l, 1)

unlink(destdir, recursive = TRUE)
}
\seealso{
\code{\link{parse_rd_db}} function for reading and parsing R-package documentation.
}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center

A.R. Trcka, U.S. Geological Survey, Idaho Water Science Center
}

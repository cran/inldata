% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_dm.R
\name{make_dm}
\alias{make_dm}
\title{Create Data Model}
\usage{
make_dm()
}
\value{
Invisibly returns the data model, an object of class \code{\link[dm:dm]{dm}}.
}
\description{
Create a data model object from \pkg{inldata} package datasets.
A data model holds a list of tables and their relationships.
Requires that the \pkg{dm} package is available.
}
\seealso{
\code{\link{make_erd}} function for creating an entity-relationship diagram.
}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}
\keyword{internal}

#' Rivers and Streams
#'
#' Stream segments in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A SpatialLinesDataFrame of the \pkg{sp} package with 197 features of 14 variables.
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source U.S. Geological Survey (USGS), National Geospatial Technical Operations Center,
#'   USGS National Hydrography Dataset (NHD) Medium Resolution for Idaho,
#'   released August 4, 2014.
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(streams, dms.tick = TRUE,
#'                  rivers = list(streams, "lwd" = 1))
#' str(streams@data)
#'
"streams"

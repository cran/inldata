#' Lakes and Ponds
#'
#' Perennial lakes and ponds in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A SpatialPolygonsDataFrame of the \pkg{sp} package with 155 features of 12 variables.
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source U.S. Geological Survey (USGS), National Geospatial Technical Operations Center,
#'   USGS National Hydrography Dataset (NHD) Medium Resolution for Idaho,
#'   released August 4, 2014.
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(lakes, dms.tick = TRUE,
#'                  lakes = list(lakes, "lwd" = 1))
#' str(lakes@data)
#'
"lakes"

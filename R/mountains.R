#' Mountain Ranges and Buttes
#'
#' Simplified representation of the mountain ranges and buttes in the
#' vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A SpatialPolygonsDataFrame of the \pkg{sp} package with 17 features and 1 variable.
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source U.S. Geological Survey
#'   \href{https://www.usgs.gov/centers/id-water/science/idaho-national-laboratory-project-office}{Idaho National Laboratory Project Office}
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(mountains, dms.tick = TRUE)
#' sp::plot(mountains, col = "gray", add = TRUE)
#' str(mountains@data)
#'
"mountains"

#' Eastern Snake River Plain Boundary
#'
#' Boundary of the eastern Snake River Plain, Idaho.
#'
#' @format A SpatialPolygonsDataFrame of the \pkg{sp} package object with 1 feature and 1 variable.
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source U.S. Geological Survey
#'   \href{https://www.usgs.gov/centers/id-water/science/idaho-national-laboratory-project-office}{Idaho National Laboratory Project Office}
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(esrp, dms.tick = TRUE)
#' sp::plot(esrp, col = "red", add = TRUE)
#' str(esrp@data)
#'
"esrp"

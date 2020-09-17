#' Idaho National Laboratory Boundary
#'
#' Boundary of the Idaho National Laboratory (\href{https://inl.gov/}{INL}).
#'
#' @format A SpatialPolygonsDataFrame of the \pkg{sp} package object with 1 feature and 4 variables.
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source U.S. Geological Survey
#'   \href{https://www.usgs.gov/centers/id-water/science/idaho-national-laboratory-project-office}{Idaho National Laboratory Project Office}
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(inl, dms.tick = TRUE)
#' sp::plot(inl, col = "red", add = TRUE)
#' str(inl@data)
#'
"inl"

#' Idaho National Laboratory Facilities
#'
#' Federal research facilities at the
#' Idaho National Laboratory (\href{https://inl.gov/}{INL}).
#'
#' @format A SpatialPolygonsDataFrame object with 7 features and 1 variable.
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source U.S. Geological Survey
#'   \href{https://www.usgs.gov/centers/id-water/science/idaho-national-laboratory-project-office}{Idaho National Laboratory Project Office}
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(facilities, dms.tick = TRUE)
#' sp::plot(facilities, col = "red", add = TRUE)
#' raster::text(facilities, facilities@data$NAME,
#'              cex = 0.6, pos = 1)
#' str(facilities@data)
#'
"facilities"

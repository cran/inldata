#' Cities and Towns
#'
#' Cities and towns (populated places) in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A SpatialPointsDataFrame of the \pkg{sp} package with 24 records and 16 variables.
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source U.S. Department of Commerce, U.S. Census Bureau, Geography Division/Cartographic Products Branch.
#'   Spatial extract from the Master Address File / Topologically Integrated Geographic Encoding and Referencing
#'   (\href{https://catalog.data.gov/dataset/tiger-line-shapefile-2019-series-information-for-the-current-place-state-based-shapefile}{MAF/TIGER}) Database (MTDB),
#'   2019 data collection, released April 2, 2020.
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(cities, dms.tick = TRUE)
#' sp::plot(cities, pch = 19, add = TRUE)
#' raster::text(cities, cities@data$NAME,
#'              pos = 1, cex = 0.6)
#' str(cities@data)
#'
"cities"

#' Primary and Secondary Roads
#'
#' Primary and secondary roads in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A SpatialLinesDataFrame of the \pkg{sp} package with 1,616 features and 5 variables.
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
#' inlmisc::PlotMap(roads, dms.tick = TRUE,
#'                  roads = list(roads, "lwd" = 1))
#' sp::plot(roads[roads@data$PRISEC, ], col = "red", add = TRUE)
#' str(roads@data)
#'
"roads"

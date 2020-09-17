#' County Boundaries
#'
#' Boundaries of counties
#' in the vicinity of Idaho National Laboratory, eastern Idaho, as of January 1, 2015.
#'
#' @format A SpatialLines of the \pkg{sp} package with 45 features.
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
#' inlmisc::PlotMap(counties, dms.tick = TRUE)
#' sp::plot(counties, lty = 5, add = TRUE)
#'
"counties"

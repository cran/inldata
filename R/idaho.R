#' State of Idaho Boundary
#'
#' Simplified representation of the boundary of Idaho,
#' a state in the northwestern region of the United States.
#'
#' @format A SpatialPolygons of the \pkg{sp} package with 1 feature.
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
#' inlmisc::PlotMap(idaho, dms.tick = TRUE)
#' sp::plot(idaho, col = "red", add = TRUE)
#'
"idaho"

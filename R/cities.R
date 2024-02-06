#' Cities and Towns
#'
#' @description Cities and towns (populated places) in the vicinity of Idaho National Laboratory,
#'   eastern Idaho as of January 1, 2022.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`name`}{City name.}
#'     \item{`id`}{Unique identifier.}
#'     \item{`geometry`}{Zero-dimensional geometry containing a single point.}
#'   }
#'   See [`crs`] dataset for coordinate reference system information.
#'
#' @source Spatial point extracts were obtained from the
#'   Master Address File / Topologically Integrated Geographic Encoding and Referencing
#'   ([MAF/TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2022.html#list-tab-ZTNQS959OZX8M0ODED))
#'   Database (MTDB), 2022 data collection, released September 30, 2022. Which is a part of the
#'   U.S. Department of Commerce, U.S. Census Bureau, Geography Division/Cartographic Products Branch.
#'   The centroids of these extracts were cropped to the study area, and any non-essential data was removed.
#'
#' @keywords datasets
#'
#' @examples
#' print(cities)
#'
#' plot(cities["name"], pch = 19)
"cities"

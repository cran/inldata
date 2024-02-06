#' Road Network
#'
#' @description Road network in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`name`}{Street or road name.}
#'     \item{`id`}{Unique identifier.}
#'     \item{`route_tp`}{Route type code that describes the type of road where
#'       "M" is the common name,
#'       "O" is other,
#'       "S" is state recognized, and
#'       "U" is United States.}
#'     \item{`prisec_fl`}{Whether a road is part of the primary road network.}
#'     \item{`geometry`}{Sequence of points connected by straight, non-self-intersecting line pieces,
#'       one-dimensional geometry.}
#'   }
#'   See [`crs`] dataset for coordinate reference system information.
#'
#' @source Spatial line extracts were obtained from the
#'   Master Address File / Topologically Integrated Geographic Encoding and Referencing
#'   ([MAF/TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2022.html#list-tab-ZTNQS959OZX8M0ODED))
#'   Database (MTDB), 2022 data collection, released September 30, 2022. Which is a part of the
#'   U.S. Department of Commerce, U.S. Census Bureau, Geography Division/Cartographic Products Branch.
#'   These lines were cropped to the study area, and any non-essential data was removed.
#'
#' @keywords datasets
#'
#' @examples
#' print(roads)
#'
#' plot(roads["prisec_fl"])
"roads"

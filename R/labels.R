#' Map Labels
#'
#' Map labels in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A SpatialPointsDataFrame of the \pkg{sp} package with 50 features and 7 variables:
#'   \describe{
#'     \item{\code{labels}}{text to be written}
#'     \item{\code{cex}}{character expansion factor}
#'     \item{\code{col}, \code{font}}{color and font to be used, respectively.}
#'     \item{\code{srt}}{string rotation in degrees.}
#'     \item{\code{halo}}{if true, a "halo" is printed around text.}
#'     \item{\code{map}}{map index}
#'   }
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source U.S. Geological Survey
#'   \href{https://www.usgs.gov/centers/id-water/science/idaho-national-laboratory-project-office}{Idaho National Laboratory Project Office}
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(labels, dms.tick = TRUE)
#' lab <- labels[labels$map == 1L, names(labels) != "map"]
#' for (i in seq_along(lab))
#'   do.call(raster::text, c(lab[i, ], as.list(lab@data[i, ])))
#' str(labels@data)
#'
"labels"

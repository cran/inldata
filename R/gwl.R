#' Groundwater Level Measurements
#'
#' Groundwater level measurements in wells in the
#' U.S. Geological Survey (USGS) water-level aquifer monitoring network,
#' Idaho National Laboratory and vicinity, Idaho.
#' Data was obtained from the National Water Information System
#' (\href{https://dx.doi.org/10.5066/F7P55KJN}{NWIS}) (U.S. Geological Survey, 2020).
#'
#' @format A data frame with 54,417 records and 5 variables:
#' \describe{
#'   \item{\code{site_no}}{USGS site number}
#'   \item{\code{lev_dt}}{date the water level was measured.}
#'   \item{\code{lev_va}}{water level value, in feet below land surface.
#'     A water-level elevation may be calculated by subtracting the depth-to-water
#'     (\code{lev_va}) from the land-surface elevation (\code{alt_va} in the \code{\link{sites}} dataset).}
#'   \item{\code{lev_status_cd}}{a code indicating the status of the site
#'     at the time the water level was measured.
#'       The codes and their meanings are:
#'       "A" water level was affected by atmospheric pressure,
#'       "D" the site was dry (no water level is recorded),
#'       "N" the measurement was discontinued,
#'       "O" an obstruction was encountered in the well (no water level was recorded),
#'       "P" the site was being pumped,
#'       "R" the site had been pumped recently,
#'       "S" a nearby site that taps the same aquifer was being pumped, and
#'       "X" the water level was affected by stage in nearby surface-water site.}
#'   \item{\code{date_time}}{date and time the water level was measured.
#'     Missing values of time were substituted with \dQuote{00:00} (12:00 midnight and start of day).}
#' }
#'
#' @source Data obtained from the NWIS database (U.S. Geological Survey, 2020).
#'
#' @references U.S. Geological Survey, 2020, National Water Information System---web services,
#'   accessed August 4, 2020, from \url{https://doi.org/10.5066/F7P55KJN}.
#'
#' @keywords datasets
#'
#' @examples
#' site_no <- "432700112470801"  # well USGS 1
#' xlim <- as.Date(c("1989-01-01", "2019-01-01"))
#' d <- gwl[gwl$site_no == site_no, c("lev_dt", "lev_va")]
#' main <- sites@data[sites@data$site_no == site_no, "site_nm"]
#' ylab <- sprintf("Water level, in %s below land surface",
#'                 c("feet", "meters"))
#' inlmisc::PlotGraph(d, ylab = ylab, main = main, xlim = xlim,
#'                    type = "p", pch = 19, seq.date.by = "year",
#'                    conversion.factor = 0.3048,
#'                    center.date.labels = TRUE)
#' str(gwl)
#'
#' alt_va <- sites@data[sites@data$site_no == site_no, "alt_va"]
#' y <- alt_va - d$lev_va
#' ylab <- sprintf("Water level, in %s above sea level",
#'                 c("feet", "meters"))
#' inlmisc::PlotGraph(d$lev_dt, y, ylab = ylab, main = main,
#'                    xlim = xlim, type = "p", pch = 19,
#'                    seq.date.by = "year",
#'                    conversion.factor = 0.3048,
#'                    center.date.labels = TRUE)
#'
"gwl"

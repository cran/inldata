#' Tabulate Site Data
#'
#' @description Tabulate data collected from sites in the U.S. Geological Survey
#'   monitoring networks at the Idaho National Laboratory and surrounding areas, Idaho.
#'
#' @param sites 'sf' objects.
#'   Descriptive site information, see [`sites`] dataset for details.
#' @param samples 'data.frame' table.
#'   Water-quality data records, see [`samples`] dataset for details.
#' @param gwl 'data.frame' table.
#'   Groundwater levels, see [`gwl`] dataset for details.
#' @param swm 'data.frame' table.
#'   Surface-water measurements, see [`swm`] dataset for details.
#'
#' @return Summary table with the following columns:
#'   \describe{
#'     \item{`network_cd`}{Local monitoring network identifier where
#'       'A' is the aquifer monitoring netowrk,
#'       'P' is the perched-groundwater monitoring network, and
#'       'S' is the surface-water monitoring network.}
#'     \item{`site_nm`}{Local monitoring site name.}
#'     \item{`site_no`}{Site identifier assigned by the USGS.}
#'     \item{`well_depth_va`}{Well depth in feet below land surface,
#'       or the sampling-port depth in feet below land surface for
#'       wells instrumented with a Multilevel Monitoring System.
#'       For surface-water sites, depth is not applicable.}
#'     \item{`min_dt`}{Date of first record.}
#'     \item{`max_dt`}{Date of last record.}
#'     \item{`nmeas`}{Number of measurments recorded at a site.}
#'     \item{`nsamples`}{Number of water-quality samples collected from the site.}
#'     \item{`nreps`}{Number of replicate samples collected from the site.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' d <- tabulate_site_data(sites, samples, gwl, swm)
#' str(d)

tabulate_site_data <- function(sites, samples, gwl, swm) {

  # check arguments
  checkmate::assert_class(sites, classes = "sf")
  checkmate::assert_data_frame(samples, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(gwl, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(swm, min.rows = 1, col.names = "named")

  dat <- sites |> as.data.frame()
  dat$geometry <- NULL

  x <- c(samples$sample_dt, gwl$lev_dt)
  by <- list("site_no" = c(samples$site_no, gwl$site_no))

  d <- stats::aggregate(x, by = by, FUN = min)
  names(d)[2] <- "min_dt"
  dat <- merge(dat, d, by = "site_no", all = TRUE)

  d <- stats::aggregate(x, by = by, FUN = max)
  names(d)[2] <- "max_dt"
  dat <- merge(dat, d, by = "site_no", all = TRUE)

  dat$min_dt <- as.Date(dat$min_dt)
  dat$max_dt <- as.Date(dat$max_dt)

  x <- table(gwl$site_no) |> as.array()
  ngwl <- x[match(dat$site_no, names(x))]

  x <- table(swm$site_no) |> as.array()
  nswm <- x[match(dat$site_no, names(x))]

  dat$nmeas <- cbind(ngwl, nswm) |>
    apply(MARGIN = 1, FUN = sum, na.rm = TRUE)

  x <- table(samples$site_no) |> as.array()
  dat$nsamples <- x[match(dat$site_no, names(x))]
  dat$nsamples[is.na(dat$nsamples)] <- 0L

  is <- !is.na(samples$rep_pair_id) &
    samples$sample_type_cd == "7"
  x <- table(samples$site_no[is]) |> as.array()
  dat$nreps <- x[match(dat$site_no, names(x))]
  dat$nreps[is.na(dat$nreps)] <- 0L

  ranks <- tolower(dat$site_nm) |> stringi::stri_rank(numeric = TRUE)
  idxs <- order(dat$network_cd, ranks, dat$well_depth_va)
  cols <- c(
    "network_cd",
    "site_nm",
    "site_no",
    "well_depth_va",
    "min_dt",
    "max_dt",
    "nmeas",
    "nsamples",
    "nreps"
  )
  dat <- dat[idxs, cols]
  rownames(dat) <- NULL

  dat
}

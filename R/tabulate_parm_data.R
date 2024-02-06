#' Tabulate Parameter Data
#'
#' @description Tabulate parameter data measured for in water samples collected from sites
#'   in the U.S. Geological Survey water-quality monitoring network,
#'   Idaho National Laboratory and vicinity, Idaho
#'
#' @param parameters 'data.frame' table.
#'   Parameter information for analytes, see [`parameters`] dataset for details.
#' @inheritParams tabulate_site_data
#'
#' @return Summary table with the following columns:
#'   \describe{
#'     \item{`parm_group_nm`}{Parameter group name.}
#'     \item{`parm_nm`}{A long parameter name assigned by the USGS.}
#'     \item{`pcode`}{Parameter code assigned by the USGS.}
#'     \item{`min_dt`}{Date of first record.}
#'     \item{`max_dt`}{Date of last record.}
#'     \item{`nrecords`}{Number of records associated with the parameter.}
#'     \item{`nsites`}{Number of sampling sites where the parameter was observed.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' d <- tabulate_parm_data(parameters, samples)
#' str(d)

tabulate_parm_data <- function(parameters, samples) {

  # check arguments
  checkmate::assert_data_frame(parameters, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(samples, min.rows = 1, col.names = "named")

  tbl <- parameters

  x <- samples$sample_dt
  by <- list("pcode" = samples$pcode)
  d <- stats::aggregate(x, by = by, FUN = min)
  names(d)[2] <- "min_dt"
  tbl <- merge(tbl, d, by = "pcode")
  d <- stats::aggregate(x, by = by, FUN = max)
  names(d)[2] <- "max_dt"
  tbl <- merge(tbl, d, by = "pcode")

  tbl$min_dt <- as.Date(tbl$min_dt)
  tbl$max_dt <- as.Date(tbl$max_dt)

  x <- table(samples$pcode) |> as.array()
  tbl$nrecords <- x[match(tbl$pcode, names(x))]

  tbl$nsites <- vapply(tbl$pcode,
    FUN = function(cd) {
      samples$site_no[samples$pcode %in% cd] |>
        unique() |>
        length()
    },
    FUN.VALUE = integer(1)
  )

  ranks <- tolower(tbl$parm_nm) |> stringi::stri_rank(numeric = TRUE)
  idxs <- order(tbl$parm_group_nm, ranks)
  cols <- c(
    "parm_group_nm",
    "parm_nm",
    "pcode",
    "min_dt",
    "max_dt",
    "nrecords",
    "nsites"
  )
  tbl <- tbl[idxs, cols]

  tbl
}

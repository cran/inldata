#' Laboratory Detection Limits
#'
#' Analytical method detection limits of selected radionuclides,
#' which are based on laboratory procedures.
#'
#' @format A data frame with 8 records and 6 variables:
#'   \describe{
#'     \item{\code{srsname}}{Substance Registry Services
#'       (\href{https://iaspub.epa.gov/sor_internet/registry/substreg/home/overview/home.do}{SRS}) name.}
#'     \item{\code{parm_cd}}{U.S. Geological Survey 5-digit parameter code.}
#'     \item{\code{parameter_units}}{parameter units}
#'     \item{\code{lab_det_lim_va}}{laboratory detection limit concentration}
#'     \item{\code{sdate}}{date that the detection limit went into effect.}
#'     \item{\code{reference}}{source of detection limit.}
#'   }
#'
#' @source Detection limits reported by:
#'   Bartholomay and others (2003, table 9);
#'   Bartholomay and others (2014, table D1); and
#'   Bodnar and Percival (1982).
#'
#' @references Bartholomay, R.C., Knobel, L.L., and Rousseau, J.P., 2003,
#'   Field methods and quality-assurance plan for quality-of-water activities,
#'   U.S. Geological Survey, Idaho National Engineering and Environmental Laboratory, Idaho:
#'   U.S. Geological Survey Open-File Report 03--42 (DOE/ID--22182), 45 p.
#'   \url{https://doi.org/10.3133/ofr0342}.
#'
#'   Bartholomay, R.C., Maimer, N.V., and Wehnke, A.J., 2014,
#'   Field methods and quality-assurance plan for water-quality activities and
#'   water-level measurements, U.S. Geological Survey, Idaho National Laboratory, Idaho:
#'   U.S. Geological Survey Open-File Report 2014--1146 (DOE/ID--22230), 64 p.
#'   \url{https://pubs.usgs.gov/of/2014/1146/}.
#'
#'   Bodnar, L.Z., and Percival, D.R., eds., 1982,
#'   Analytical Chemistry Branch procedures manual---Radiological and Environmental Sciences Laboratory:
#'   U.S. Department of Energy Report IDO--12096 [variously paged].
#'
#' @keywords datasets
#'
#' @examples
#' str(dl)
#'
"dl"

#' Background Concentrations
#'
#' Water-quality background concentrations for
#' selected radionuclides, organic compounds, and chemical constituents
#' analyzed for in water from the Eastern Snake River Plain aquifer at and near
#' the Idaho National Laboratory (INL).
#' Background concentrations are either naturally occurring or anthropogenic,
#' and are not influenced by waste and wastewater disposal at the INL
#' (Bartholomay and Hall, 2016, p. 3).
#'
#' @format A data frame with 73 records and 4 variables:
#'   \describe{
#'     \item{\code{parm_cd}}{U.S. Geological Survey 5-digit parameter code,
#'       see \code{\link{parameters}} dataset for details.}
#'     \item{\code{bkgrd_min}, \code{bkgrd_max}}{minimum and maximum limits of background concentration}
#'     \item{\code{reference}}{source of background concentration limits.
#'       Reference citations are:
#'       \code{"Bartholomay and Hall (2016)"},
#'       \code{"Knobel and others (1992)"},
#'       \code{"Michel (1989)"}, and
#'       \code{"Orr and others (1991)"}.}
#'   }
#'   Row names for the data frame indicate the Substance Registry Services
#'   (\href{https://iaspub.epa.gov/sor_internet/registry/substreg/home/overview/home.do}{SRS})
#'   name and parameter units.
#'
#' @references Bartholomay, R.C., and Hall, L.F., 2016, Evaluation of background concentrations
#'   of selected chemical and radiochemical constituents in groundwater in the
#'   eastern Snake River Plain aquifer at and near the Idaho National Laboratory, Idaho:
#'   U.S. Geological Survey Scientific Investigations Report 2016--5056, (DOE/ID--22237), 19 p.,
#'   \url{https://doi.org/10.3133/sir20165056}.
#'
#'   Knobel, L.L., Orr, B.R., and Cecil, L.D., 1992, Summary of background concentrations of
#'   selected radiochemical and chemical constituents in groundwater from the Snake River
#'   Plain aquifer, Idaho: estimated from an analysis of previously published data:
#'   Journal of the Idaho Academy of Science, v. 28, no. 1, p. 48--61.
#'
#'   Michel, R.L., 1989, Tritium deposition in the continental United States, 1953--83:
#'   U.S. Geological Survey Water Resources Investigations Report 89--4072, 46 p.,
#'   \url{https://doi.org/10.3133/wri894072}.
#'
#'   Orr, B.R., Cecil, L.D., and Knobel, L.L., 1991, Background concentrations of
#'   selected radionuclides, organic compounds, and chemical constituents in
#'   ground water in the vicinity of the Idaho National Engineering Laboratory:
#'   U.S. Geological Survey Water-Resources Investigations Report 91--4015 (DOE/ID--22094), 52 p.,
#'   \url{https://doi.org/10.3133/wri914015}.
#'
#' @keywords datasets
#'
#' @examples
#' str(background)
#' rownames(background)
#'
"background"

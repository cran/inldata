#' Parameter Information for Analytes
#'
#' Parameter code information for selected chemical constituents,
#' organic compounds, and radionuclides measured for in water samples collected from
#' wells in the U.S. Geological Survey (USGS) water-quality aquifer monitoring network,
#' Idaho National Laboratory and vicinity, Idaho.
#'
#' @format A data frame with 74 records and 7 variables:
#'   \describe{
#'     \item{\code{parm_cd}}{USGS 5-digit parameter code}
#'     \item{\code{parameter_group_nm}}{parameter group name, such as \code{"Radiochemical"}}
#'     \item{\code{parameter_nm}}{long parameter name, such as \code{"Strontium-90, water, unfiltered, picocuries per liter"}}
#'     \item{\code{casrn}}{Chemical Abstracts Service
#'       (\href{https://www.cas.org/support/documentation/chemical-substances/faqs}{CAS}) registry number,
#'       such as \code{"10098-97-2"} for Strontium-90}
#'     \item{\code{srsname}}{Substance Registry Services
#'       (\href{https://iaspub.epa.gov/sor_internet/registry/substreg/home/overview/home.do}{SRS}) name,
#'       such as \code{"Strontium-90"}}
#'     \item{\code{parameter_units}}{parameter units.
#'       Unit abbreviations and descriptions are:
#'       \code{"mg/L"} milligrams per liter,
#'       \code{"mg/L as N"} milligrams per liter as Nitrogen,
#'       \code{"ug/L"} microgram per liter, and
#'       \code{"pCi/L"} picocuries per liter.}
#'     \item{\code{siunitx}}{parameter units formatted for LaTeX using the \code{siunitx} package notation.}
#'   }
#'
#' @source USGS water data acquired from the National Water Information System (U.S. Geological Survey, 2019).
#'   The \href{https://iaspub.epa.gov/sor_internet/registry/substreg/home/overview/home.do}{SRS} name (\code{srsname})
#'   for "Trihalomethanes (four), total, from SDWA NPDWR" was shorten to its preferred acronym "TTHM4".
#'
#' @references U.S. Geological Survey, 2019, National Water Information System---web services,
#'   accessed June 11, 2019, from \url{https://doi.org/10.5066/F7P55KJN}.
#'
#' @keywords datasets
#'
#' @examples
#' str(parameters)
#'
"parameters"

#' Benchmark Concentrations
#'
#' Water-quality benchmark concentrations of
#' selected radionuclides, organic compounds, and chemical constituents.
#' Benchmarks include:
#' the U.S. Environmental Protection Agency (USEPA) Maximum Contaminant Levels
#' (\href{https://www.epa.gov/ground-water-and-drinking-water/national-primary-drinking-water-regulations}{MCLs}) and
#' Human Health Benchmarks for Pesticides
#' (\href{https://iaspub.epa.gov/apex/pesticides/f?p=HHBP:home}{HHBPs}), and
#' the U.S. Geological Survey (USGS) Health-Based Screening Levels
#' (\href{https://water.usgs.gov/water-resources/hbsl/}{HBSLs}).
#'
#' @format A data frame with 74 records and 9 variables:
#'   \describe{
#'     \item{\code{parm_cd}}{USGS 5-digit parameter code, see \code{\link{parameters}} dataset for details.}
#'     \item{\code{mcl}}{USEPA MCLs.}
#'     \item{\code{hhbp_noncancer}}{USEPA Chronic Noncancer HHBPs}
#'     \item{\code{hhbp_cancer_min}}{USEPA Carcinogenic HHBPs for a one-in-one million cancer risk.}
#'     \item{\code{hhbp_cancer_max}}{USEPA Carcinogenic HHBPs for a one-in-ten thousand cancer risk.}
#'     \item{\code{hbsl_noncancer}}{USGS Noncancer HBSLs}
#'     \item{\code{hbsl_cancer_min}}{USGS Cancer HBSLs for a one-in-one million cancer risk.}
#'     \item{\code{hbsl_cancer_max}}{USGS Cancer HBSLs for a one-in-ten thousand cancer risk.}
#'     \item{\code{remark}}{comments}
#'   }
#'   Row names for the data frame indicate the Substance Registry Services
#'   (\href{https://iaspub.epa.gov/sor_internet/registry/substreg/home/overview/home.do}{SRS})
#'   name and parameter units.
#'
#' @source Many of the water-quality benchmarks were accessed from the
#'   U.S. Geological Survey Health-Based Screening Levels database,
#'   accessed on August 8, 2017, from \url{https://water.usgs.gov/water-resources/hbsl/}.
#'   Benchmarks for total Trihalomethanes, Tritium, and Strontium-90 were
#'   provided by the U.S. Environmental Protection Agency (2015).
#'   Note that MCL benchmark values reported in millirem per year were
#'   substituted with a 50 picocuries per liter screening level.
#'
#' @references U.S. Environmental Protection Agency, 2015,
#'   Protection of environment---Code of Federal Regulations 40,
#'   Part 141, Subpart G, National Primary Drinking Water Regulations,
#'   Maximum Contaminant Levels and Maximum Residual Disinfectant Levels:
#'   Washington, D.C., Office of the Federal Register,
#'   National Archives and Records Administration.
#'
#' @keywords datasets
#'
#' @examples
#' str(benchmarks)
#' rownames(benchmarks)
#'
"benchmarks"

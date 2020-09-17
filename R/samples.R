#' Water-Quality Data Records
#'
#' Water-quality data from laboratory analyses of groundwater samples
#' collected from wells in the U.S. Geological Survey (USGS) water-quality monitoring network,
#' Idaho National Laboratory and vicinity, Idaho.
#' Data was obtained from the National Water Information System (U.S. Geological Survey, 2019).
#'
#' @format A data frame with 159,554 records and 19 variables:
#' \describe{
#'   \item{\code{site_no}}{USGS site identification number}
#'   \item{\code{sample_dt}}{date sample was collected}
#'   \item{\code{medium_cd}}{3-digit medium code that identifies the material type and quality assurance type of the sample.
#'     The codes and their meanings are:
#'     \code{"WG"} water below land surface contained in the saturated zone (groundwater); and
#'     \code{"WGQ"} groundwater quality-control (QC) sample.}
#'   \item{\code{db_no}}{2-digit NWIS database number.
#'     The codes and their meanings are:
#'     \code{"01"} is the environmental database, and
#'     \code{"10"} is the quality-assurance (QA) database.}
#'   \item{\code{anl_ent_cd}}{analyzing entity code of the organizational unit that
#'     performed the sample analysis used to obtain the result.}
#'   \item{\code{parm_cd}}{USGS 5-digit parameter code.
#'     For example, the parameter code for Tritium is \code{"07000"}.}
#'   \item{\code{remark_cd}}{1-digit remark code (result level) used to qualify the parameter value.
#'     The codes and their meanings are:
#'     \code{""} quantified value.
#'     \code{"<"} actual value is known to be less than the value reported,
#'       that is, the measured concentration is below the reporting limit (RL) and represented as a censored (or nondetection) value.
#'       For censored values, the value reported is the RL.
#'     \code{"E"} value is estimated, that is, the actual value is greater than the minimum detection limit (MDL)
#'       and less than the laboratory reporting level (LRL).
#'     \code{"R"} nondetect, result less than sample-specific critical level.
#'     \code{"U"} material specifically analyzed for but not detected.
#'     \code{"V"} value affected by contamination.}
#'   \item{\code{result_va}}{parameter value;
#'     see \code{parameter_units} variable in the \code{\link{parameters}} dataset for the units of measurement.}
#'   \item{\code{lab_std_dev_va}}{laboratory standard deviation (SD).
#'     For radiochemical data, SD is determined from the counting error.
#'     Prior to January 1, 2018, counting error was reported as two SD, therefore, these values were divied by 2.}
#'   \item{\code{dqi_cd}}{1-digit data quality indicator code that indicates the review status of a result.
#'     The codes and their meanings are:
#'     \code{"A"} historical data,
#'     \code{"S"} presumed satisfactory, and
#'     \code{"R"} reviewed and accepted.}
#'   \item{\code{rpt_lev_va}}{laboratory reporting limit in effect for the parameter and
#'     method at the time the measurement was made.}
#'   \item{\code{rpt_lev_cd}}{reporting level code that identifies the analytical reporting level appropriate for the analytical method.
#'     The codes and their meanings are:
#'     \code{"DLBLK"} detection limit by blank data;
#'     \code{"DLDQC"} detection limit by DQCALC, lowest concentration that with 90 percent confidence will be exceeded no more than
#'       1 percent of the time when a blank sample is measured;
#'     \code{"IRL"} interim reporting level, a temporary reporting level;
#'     \code{"LRL"} laboratory reporting level, equal to twice the yearly-determined LT-MDL;
#'     \code{"LT-MDL"} long-term method detection limit, a detection level derived by determining the standard deviation of
#'       a minimum of 24 MDL spike sample measurements over an extended period of time;
#'     \code{"MDL"} method detection limit, minimum concentration of a substance that can be measured and reported with
#'       a 99 percent confidence that the analyte concentration in greater than zero;
#'     \code{"PQL"} practical quantitation limits;
#'     \code{"MRL"} minimum reporting level, smallest measured concentration that can be reliably measured using
#'       a given analytical method;
#'     \code{"RLDQC"} reporting limit by DQCALC, is greater than or equal to two times the DLDQC;
#'     \code{"SSLC"} sample-specific critical level, the calculated and reported value is below which the radiochemistry result
#'       is considered a non-detect; and
#'     \code{"SSMDC"} sample-specific minimum detectable concentration, a reporting level that varies for each sample and is primarily used
#'       in radiochemical analyses.}
#'   \item{\code{samp_type_cd}}{1-digit sample type code that identifies the QA type of a sample.
#'     The codes and their meanings are:
#'     \code{"H"} samples collected over a period of time (composite);
#'     \code{"2"} samples prepared from a reference material where none of the analytes of interest are
#'       present in detectable quantities (blank);
#'     \code{"6"} reference material that provide the baseline analytical results for comparison with reference samples;
#'     \code{"7"} replicate samples; and
#'     \code{"9"} sample taken from the environment (regular).}
#'   \item{\code{meth_cd}}{method code, the codes are documented in the
#'     "\href{https://help.waterdata.usgs.gov/code/method_cd_query}{NWIS Method Code Dictionary}"}
#'   \item{\code{result_cm_tx}}{comment about the water quality result}
#'   \item{\code{date_time}}{date and time the sample was collected.
#'     Missing values of time were substituted with \dQuote{00:00} (12:00 midnight and start of day).}
#'   \item{\code{sample_cd}}{unique identifier for the water sample.
#'     The sample code is a concatenation of the \code{date_time} and \code{site_no} variables.}
#'   \item{\code{comments}}{comments pertaining to changes applied after the records were obtained from NWIS.}
#'   \item{\code{rep_pair_no}}{number used to identify replicate samples.
#'     Replicates are identified by matching a replicate sample (\code{samp_type_cd} eqaul to 7) with
#'     its corresponding environmental sample (\code{samp_type_cd} equal to 9).}
#' }
#'
#' @source Data obtained from the NWIS-QWDATA database on June 11, 2019 using the QWDATA system (U.S. Geological Survey, 2019).
#'
#' @references
#'   U.S. Geological Survey, 2019, National Water Information System---Water-Quality System (QWDATA)
#'   data retrieval program.
#'
#' @keywords datasets
#'
#' @examples
#' site_no <- "433002113021701"  # well RWMC PROD
#' parm_cd <- "32102"            # carbon tetrachloride
#' xlim <- as.Date(c("1989-01-01", "2019-01-01"))
#' d <- samples[samples$site_no == site_no & samples$parm_cd == parm_cd,
#'              c("sample_dt", "result_va")]
#' ylab <- parameters[parameters$parm_cd == parm_cd, "parameter_nm"]
#' main <- sites@data[sites@data$site_no == site_no, "site_nm"]
#' inlmisc::PlotGraph(d, ylab = ylab, main = main, xlim = xlim,
#'                    type = "p", pch = 19, seq.date.by = "year",
#'                    center.date.labels = TRUE)
#' str(samples)
#'
"samples"

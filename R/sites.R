#' Descriptive Site Information
#'
#' Information for sampling sites in the U.S. Geological Survey (USGS)
#' aquifer monitoring networks, Idaho National Laboratory and vicinity, Idaho.
#'
#' @format A SpatialPointsDataFrame of the \pkg{sp} package with 182 records and 21 variables:
#'   \describe{
#'     \item{\code{site_no}}{USGS site number for monitoring well}
#'     \item{\code{station_nm}}{USGS site name}
#'     \item{\code{coord_meth_cd}}{latitude/longitude coordinate method code.
#'       The codes and their meanings are:
#'       "D" differentially corrected Global Positioning System (GPS),
#'       "G" mapping grade GPS unit (handheld accuracy range 3.7 to 12.2 meters),
#'       "L" long range navigation system,
#'       "M" interpolated from topographic map, and
#'       "S" transit, theodolite, or other surveying method.}
#'     \item{\code{coord_acy_cd}}{accuracy code for latitude/longitude values.
#'       The codes and their meanings are:
#'       "H" accurate to \eqn{\pm}{+/-} 0.1 second,
#'       "5" accurate to \eqn{\pm}{+/-} 0.5 second,
#'       "S" accurate to \eqn{\pm}{+/-} 1 second, and
#'       "F" accurate to \eqn{\pm}{+/-} 5 seconds.}
#'     \item{\code{alt_va}}{elevation of the land surface reference point,
#'       in feet above the North American Vertical Datum of 1988.}
#'     \item{\code{alt_meth_cd}}{method code for measuring elevation.
#'       The codes and their meanings are:
#'       "D" differentially corrected global positioning system,
#'       "L" level or other surveyed method, and
#'       "M" interpolated from topographic map.}
#'     \item{\code{alt_acy_va}}{accuracy of the elevation value (alt_va),
#'       does not account for vertical datum shift.}
#'     \item{\code{construction_dt}}{date the well was completed.}
#'     \item{\code{huc_cd}}{hydrologic unit code (HUC).
#'       Hydrologic units are geographic areas representing part or all of a surface drainage basin or
#'       distinct hydrologic feature and are delineated on the
#'       \href{https://water.usgs.gov/GIS/regions.html}{Hydrologic Unit Map}.}
#'     \item{\code{reliability_cd}}{reliability code for data available for the site.
#'       The codes and their meanings are:
#'       "C" data have been checked by the reporting agency, and
#'       "U" unchecked data.}
#'     \item{\code{nat_aqfr_cd}}{national aquifer codes}
#'     \item{\code{aqfr_cd}}{aquifer codes defined by the catalog of aquifer names and
#'       geologic unit codes used by the Water Mission Area.}
#'     \item{\code{aqfr_type_cd}}{aquifer type code.
#'       The codes and their meanings are:
#'       "C" confined single aquifer,
#'       "M" confined multiple aquifers,
#'       "U" unconfined single aquifer, and
#'       "X" mixed (confined and unconfined) multiple aquifers.}
#'     \item{\code{well_depth_va}}{depth of the finished well, in feet below the land surface datum}
#'     \item{\code{hole_depth_va}}{total depth of the borehole, in feet below the land surface datum}
#'     \item{\code{depth_src_cd}}{source code for depth measurements.
#'       The codes and their meanings are:
#'       "A" reported by another government agency,
#'       "D" from driller's log or report,
#'       "G" private geologist-consultant or university associate,
#'       "L" interpreted from geophysical logs by personnel of source agency,
#'       "O" reported by owner of well,
#'       "R" reported by person other than the owner, driller, or another government agency, and
#'       "S" measured by personnel of reporting agency.}
#'     \item{\code{site_nm}}{local site name}
#'     \item{\code{completion_cd}}{borehole completion code.
#'       The codes and their meanings are:
#'       "O" open hole completion,
#'       "M" multilevel completion, and
#'       "P" open hole completion prior to multilevel completion.}
#'     \item{\code{network}}{aquifer monitoring network,
#'       either "aquifer" or "perched".}
#'     \item{\code{pos}}{a position specifier for site-labels on a map.
#'       Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of,
#'       above and to the right of the site coordinates.}
#'   }
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source USGS site data acquired from the NWIS (U.S. Geological Survey, 2020).
#'
#' @references U.S. Geological Survey, 2020, National Water Information System---web services,
#'   accessed August 4, 2020, from \url{https://doi.org/10.5066/F7P55KJN}.
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(sites, dms.tick = TRUE)
#' sp::plot(sites, add = TRUE)
#' str(sites@data)
#'
"sites"
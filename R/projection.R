#' Coordinate Reference System
#'
#' \href{https://proj4.org/}{PROJ.4} string defining a
#' custom coordinate reference system (CRS) used by the
#' U.S. Geological Survey Idaho National Laboratory Project Office.
#' The CRS is based on the following attributes:
#' Albers equal-area conic projection;
#' latitude of first and second standard parallel is 42.83 and 44.16 decimal degrees, respectively;
#' latitude and longitude of false origin is 41.5 and -113 decimal degrees, respectively;
#' easting and northing of false origin is 200,000 and 0 meters, respectively;
#' Clarke (1966) reference ellipsoid;
#' North American Datum of 1983; and
#' units of meters.
#'
#' @format A character string describing the CRS in PROJ.4 format.
#'
#' @keywords datasets
#' @examples
#' crs <- sp::CRS(projection)  # convert to class "CRS"
#' print(crs)
#'
"projection"

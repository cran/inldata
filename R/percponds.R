#' Percolation Ponds
#'
#' @description Percolation ponds in the vicinity of Idaho National Laboratory (INL), eastern Idaho.
#'   Wastewater from facilities at the INL has been and is currently disposed of in percolation (infiltration),
#'   evaporation (lined and unlined), and infiltration ponds.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`name`}{Name of the percolation ponds.}
#'     \item{`facility_id`}{INL facility the percolation pond is located at.
#'       Abbreviations and descriptions are as follows:
#'       "ATRC" is the Advanced Test Reactor Complex,
#'       "INTEC" is the Idaho Nuclear Technology and Engineering Center,
#'       "MFC" is the Materials and Fuels Complex,
#'       "NRF" is the Nuclear Reactors Facility, and
#'       "TAN" is the Test Area North facility.}
#'     \item{`geometry`}{Polygon geometry with a positive area (two-dimensional);
#'       sequence of points that form a closed, non-self-intersecting ring; the first ring denotes the exterior ring,
#'       zero or more subsequent rings denote holes in this exterior ring.}
#'   }
#'
#' @source U.S. Geological Survey
#'   [Idaho National Laboratory Project Office](https://www.usgs.gov/centers/idaho-water-science-center/science/idaho-national-laboratory-project-office)
#'
#' @keywords datasets
#'
#' @examples
#' print(percponds)
#'
#' plot(percponds["name"], col = "lightblue", border = "blue")
"percponds"

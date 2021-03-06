#' Land-Surface Topography
#'
#' Land-surface topography and shaded relief
#' in the vicinity of Idaho National Laboratory, eastern Idaho.
#'
#' @format A RasterStack of the \pkg{raster} package with 2 layers.
#'   Grid cells in the \code{"elevation"} layer represent land-surface elevations in meters above the
#'   North American Vertical Datum of 1988 (NAVD 88).
#'   Grid cells in the \code{"hillshade"} layer represent shaded relief calculated from
#'   the slope and aspect of land-surface elevations.
#'   The spatial grid is composed of 900 rows and 900 columns,
#'   and has cell sizes that are constant at 100 meters by 100 meters.
#'   See \code{\link{projection}} dataset for coordinate reference system information.
#'
#' @source The National Map (\href{https://www.usgs.gov/core-science-systems/national-geospatial-program/national-map}{TNM})
#'   1/3-arc-second DEM (Gesch, 2007; Gesch and others, 2002), accessed on August 4, 2020.
#'   This dataset can be downloaded in a Esri ArcGRID \eqn{\textsuperscript{TM}}{™} format using
#'   \href{https://viewer.nationalmap.gov/basic/}{TNM Download}.
#'   Elevation datasets are distributed in geographic coordinates in units of decimal degrees,
#'   and in conformance with the NAD 83.
#'   Elevation values are in meters above the NAVD 88.
#'   The west, east, south, and north bounding coordinates for this dataset are
#'   -114, -112, 43, and 45 decimal degrees, respectively.
#'
#' @references Gesch, D.B., 2007, The National Elevation Dataset, in Maune, D., ed.,
#'   Digital Elevation Model Technologies and Applications---The DEM Users Manual,
#'   2nd ed.: Bethesda, Maryland, American Society for Photogrammetry and Remote Sensing,
#'   p. 99--118.
#'
#'   Gesch, D., Oimoen, M., Greenlee, S., Nelson, C., Steuck, M., and Tyler, D., 2002,
#'   The National Elevation Dataset: Photogrammetric Engineering and Remote Sensing,
#'   v. 68, no. 1, p. 5--11.
#'
#' @keywords datasets
#'
#' @examples
#' inlmisc::PlotMap(topo[["elevation"]], bg.image = topo[["hillshade"]],
#'                  pal = inlmisc::GetColors(scheme = "dem3", alpha = 0.5),
#'                  dms.tick = TRUE, useRaster = TRUE)
#'
"topo"

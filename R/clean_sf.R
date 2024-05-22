#' Clean a Simple Feature
#'
#' @description Subset columns, transform coordinates,
#'   and (or) crop the spatial extent of a simple feature.
#'
#' @param x 'sf' object.
#'   Simple feature.
#' @param cols 'character' vector.
#'   Names indicating the columns to keep.
#'   Vector names are optional and used to rename columns.
#' @param agr 'character' vector.
#'   Attribute geometry relationship,
#'   specifies for each non-geometry attribute column how it relates to the geometry,
#'   and can have one of following values: "constant", "aggregate", "identity".
#'   Where "constant" is used for attributes that are constant throughout the geometry,
#'   "aggregate" where the attribute is an aggregate value over the geometry,
#'   "identity" when the attributes uniquely identifies the geometry of particular thing.
#'   The default value assumes you don't know.
#'   If named, vector names should correspond to the non-geometry list-column columns of `x`.
#' @param crs 'crs' object.
#'   Target coordinate reference system.
#' @param extent 'bbox' object.
#'   Spatial extent (aka bounding box) used to crop the simple feature.
#'
#' @return Returns an object of class 'sf'.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal

clean_sf <- function(x,
                     cols = NULL,
                     agr = NULL,
                     crs = NULL,
                     extent = NULL) {

  # check arguments
  checkmate::assert_multi_class(x, classes = c("sf", "data.frame"))
  checkmate::assert_character(cols,
    any.missing = FALSE,
    min.len = 1,
    max.len = ncol(x),
    null.ok = TRUE
  )
  checkmate::assert_character(agr,
    any.missing = FALSE,
    min.len = 1,
    max.len = ncol(x) - 1L,
    null.ok = TRUE
  )
  checkmate::assert_class(crs, classes = "crs", null.ok = TRUE)
  checkmate::assert_class(extent, classes = "bbox", null.ok = TRUE)

  # make valid
  sp <- sf::st_make_valid(x)

  # transform coordinates
  if (!is.null(crs)) {
    sp <- sf::st_transform(sp, crs = crs, check = TRUE)
  }

  # subset columns
  if (!is.null(cols)) {
    checkmate::assert_subset(cols, choices = colnames(sp))
    sp <- sp[, cols]

    # rename columns
    if (!is.null(names(cols))) {
      names(sp) <- names(cols)
    }
  }

  # set relation to geometry attribute
  if (!is.null(agr)) {
    checkmate::assert_subset(agr,
      choices = c("constant", "aggregate", "identity")
    )
    if (!is.null(names(agr))) {
      checkmate::assert_subset(names(agr), choices = colnames(sp))
    }
    sf::st_agr(sp) <- agr
  }

  # crop to extent
  if (!is.null(extent)) {
    bb <- sf::st_as_sfc(extent) |>
      sf::st_transform(crs = sf::st_crs(sp)) |>
      sf::st_bbox()
    sp <- sf::st_crop(sp, bb)
  }

  # check validity
  sf::st_is_valid(sp) |>
    stopifnot()

  # clear row names
  rownames(sp) <- NULL

  sp
}

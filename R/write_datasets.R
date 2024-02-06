#' Write Package Datasets
#'
#' @description Write content of package datasets to non-proprietary, open, documented standard
#'   file formats that are more likely to be accessible in the future.
#'   Requires that the \pkg{jsonlite} package is available.
#'
#' @param package 'character' string.
#'   Name of a package.
#' @param destdir 'character' string.
#'   Destination directory to write files, with tilde-expansion performed.
#' @param overwrite 'logical' flag.
#'   Whether to overwrite an existing file.
#' @param include 'character' vector.
#'   Names(s) of datasets to include.
#'   By default, a file is created for each package dataset.
#' @param exclude 'character' vector.
#'   Name(s) of datasets to exclude.
#'   By default, none are excluded.
#' @param pretty 'logical' flag.
#'   Whether to add indentation, whitespace, and newlines to JSON output (default is `TRUE`).
#'   See [`prettify`][jsonlite::prettify] function for details.
#'   The tradeoff for human-readable output is a much larger file size.
#' @param quiet 'logical' flag.
#'   Whether to suppress printing of debugging information.
#'
#' @details A simple-feature dataset (class 'sf' or 'sfc') is written to a GeoJSON file.
#'   A spatial-raster dataset (class 'SpatRaster' or 'PackedSpatRaster') is written to a GeoTIFF file.
#'   A non-spatial dataset is written to a JSON file.
#'
#' @return Invisibly returns the output file path(s).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' dir <- tempfile("")
#' write_datasets(
#'   package = "inldata",
#'   destdir = dir,
#'   include = c("crs", "dl", "inl"),
#'   pretty = FALSE,
#'   quiet = TRUE
#' )
#'
#' unlink(dir, recursive = TRUE)

write_datasets <- function(package,
                           destdir = getwd(),
                           overwrite = TRUE,
                           include = NULL,
                           exclude = NULL,
                           pretty = TRUE,
                           quiet = FALSE) {

  # check packages
  check_package(pkg = "jsonlite", msg = "Writing datasets")

  # check arguments
  checkmate::assert_string(package)
  checkmate::test_string(destdir, min.chars = 1)
  checkmate::assert_logical(overwrite)
  checkmate::assert_character(include,
    any.missing = FALSE,
    unique = TRUE,
    min.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_character(exclude,
    any.missing = FALSE,
    unique = TRUE,
    min.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_logical(pretty)
  checkmate::assert_logical(quiet)

  # make output directory
  destdir <- path.expand(destdir) |>
    normalizePath(winslash = "/", mustWork = FALSE)
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

  # get package dataset names
  ds_names <- utils::data(package = package)$results[, "Item"]

  # include datasets
  if (!is.null(include)) {
    checkmate::assert_subset(include, choices = ds_names)
    ds_names <- ds_names[ds_names %in% include]
  }

  # exclude datasets
  if (!is.null(exclude)) {
    checkmate::assert_subset(exclude, choices = ds_names)
    ds_names <- ds_names[!ds_names %in% exclude]
  }

  # stop when no datasets exist
  if (length(ds_names) == 0L) {
    stop("No datasets for package ", package)
  }

  # initialize vector of output paths
  paths <- character(0)

  # loop through datasets
  for (name in ds_names) {

    # print status message
    if (!quiet) {
      sprintf("STATUS: writing %s::%s dataset ...", package, name) |> message()
    }

    # save dataset to a temporary object
    envir <- new.env()
    nm <- utils::data(list = name, package = package, envir = envir)[1]
    obj <- envir[[nm]]

    # write simple feature dataset to GeoJSON file
    if (inherits(obj, c("sf", "sfc"))) {
      crs <- sf::st_crs(obj)
      if (!sf::st_can_transform(src = crs, dst = 4326)) {
        stop("Unable to convert coordinates to WGS 84 (EPSG:4326)")
      }

      # format data-time values, a workaround for GeoJSON driver not formatting dates correctly
      is <- vapply(obj,
        FUN = checkmate::test_multi_class,
        FUN.VALUE = logical(1),
        classes = c("Date", "POSIXt")
      )
      for (i in which(is)) obj[[i]] <- format(obj[[i]])

      path <- sprintf("%s/%s.geojson", destdir, name)
      checkmate::assert_path_for_output(path, overwrite = overwrite)
      sf::st_transform(obj, crs = 4326) |>
        sf::st_write(
          dsn = path,
          layer = basename(path),
          overwrite = TRUE,
          quiet = quiet,
          delete_dsn = checkmate::test_file_exists(path, access = "rw")
        )
      txt <- readLines(con = path, encoding = "UTF-8")
      txt <- if (pretty) jsonlite::prettify(txt, indent = 2) else jsonlite::minify(txt)
      writeLines(text = txt, con = path, useBytes = TRUE)

    # write spatial raster dataset to GeoTIFF
    } else if (inherits(obj, c("SpatRaster", "PackedSpatRaster"))) {
      path <- sprintf("%s/%s.tif", destdir, name)
      checkmate::assert_path_for_output(path, overwrite = overwrite)
      terra::unwrap(obj) |>
        terra::writeRaster(filename = path, overwrite = TRUE, verbose = !quiet)

    # write coordinate reference system to text
    } else if (inherits(obj, "crs")) {
      path <- sprintf("%s/%s.txt", destdir, name)
      writeLines(obj$wkt, con = path)

    # write dataset to JSON file
    } else {
      path <- sprintf("%s/%s.json", destdir, name)
      checkmate::assert_path_for_output(path, overwrite = overwrite)
      jsonlite::toJSON(obj, null = "null", na = "null", digits = 8, pretty = pretty) |>
        writeLines(con = path, useBytes = TRUE)
    }

    # append path
    paths <- c(paths, path)
  }

  # return output paths
  invisible(paths)
}

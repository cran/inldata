#' Write Package Datasets
#'
#' @description Exports the content of package datasets into non-proprietary, open,
#'   and well-documented standard file formats, enhancing their accessibility for future use.
#'   It supports multiple formats:
#'   CSV,
#'   JSON (requires `jsonlite` package),
#'   Arrow Parquet (requires `arrow` package),
#'   Excel XLMS (requires `writexl` package),
#'   GeoJSON,
#'   Shapefile, and
#'   GeoTIFF.
#'
#' @param package 'character' string.
#'   Name of a package.
#' @param destdir 'character' string.
#'   Destination directory to write files, with tilde-expansion performed.
#' @param formats 'character' vector.
#'   Formats for saving datasets.
#'   Choose from one or more of the following formats:
#'   `csv`, `geojson`, `json`, `parquet`, `shp`, `tiff`, `txt`, and `xlsx`.
#'   Please refer to the *details* section for a description of each format.
#'   All file formats are saved to the disk by default.
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
#' @details
#'   Different types of datasets are written to various file formats.
#'   For instance, a dataset in the form of a data table (`data.frame` class) is exported to
#'   a CSV (`csv`), JSON (`json`), Parquet (`parquet`), and Spreadsheet (`xlsx`) file.
#'   A dataset representing simple features (`sf` or `sfc` class) is exported to
#'   a GeoJSON (`geojson`) and Shapefile (`shp`) file.
#'   A spatial-raster dataset (`SpatRaster` or `PackedSpatRaster` class) is exported to
#'   a GeoTIFF (`tiff`) file.
#'   Lastly, a coordinate reference system (`crs` class) is exported to a text (`txt`) file.
#'
#' @return Invisibly returns the output file path(s).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
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
                           formats = NULL,
                           overwrite = TRUE,
                           include = NULL,
                           exclude = NULL,
                           pretty = TRUE,
                           quiet = FALSE) {

  # check arguments
  checkmate::assert_string(package)
  checkmate::assert_string(destdir, min.chars = 1)
  checkmate::assert_character(formats,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    null.ok = TRUE
  )
  choices <- c("csv", "geojson", "json", "parquet", "shp", "tiff", "txt", "xlsx")
  if (is.null(formats)) {
    formats <- choices
  }
  checkmate::assert_subset(formats, choices = choices)
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

  # check packages
  pkgs <- c(
    "parquet" = "arrow",
    "json" = "jsonlite",
    "xlsx" = "writexl"
  )
  for (pkg in pkgs[names(pkgs) %in% formats]) {
    check_package(pkg = pkg, msg = "Writing datasets")
  }

  # make output directory
  destdir <- path.expand(destdir) |>
    normalizePath(winslash = "/", mustWork = FALSE)
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

  # get package dataset names
  ds_names <- utils::data(
    package = package,
    verbose = !quiet
  )$results[, "Item"]

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
      sprintf("STATUS: writing %s::%s dataset ...", package, name) |>
        message()
    }

    # save dataset to a temporary environment
    envir <- new.env()
    nm <- utils::data(
      list = name,
      package = package,
      verbose = !quiet,
      envir = envir
    )[1]
    ds <- envir[[nm]]

    # simple feature class
    if (inherits(ds, c("sf", "sfc"))) {

      # format data-time values
      is <- vapply(ds,
        FUN = checkmate::test_multi_class,
        FUN.VALUE = logical(1),
        classes = c("Date", "POSIXt")
      )
      for (i in which(is)) {
        ds[[i]] <- format(ds[[i]])
      }

      # write GeoJSON file
      if ("geojson" %in% formats) {
        path <- sprintf("%s/%s.geojson", destdir, name)
        checkmate::assert_path_for_output(path, overwrite = overwrite)
        crs <- sf::st_crs(ds)
        if (!sf::st_can_transform(src = crs, dst = 4326)) {
          stop("Unable to convert coordinates to WGS 84 (EPSG:4326)")
        }
        sf::st_transform(ds, crs = 4326) |>
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
      }

      # write compressed SHP files
      if ("shp" %in% formats) {
        dir <- tempfile("")
        dir.create(dir, showWarnings = FALSE, recursive = TRUE)
        dsn <- sprintf("%s/%s.shp", dir, name)
        sf::st_write(ds, dsn = dsn, quiet = quiet)
        files <- list.files(
          path = dirname(dsn),
          pattern = paste0(name, ".*$"),
          full.names = TRUE
        )
        path <- sprintf("%s/%s.zip", destdir, name)
        checkmate::assert_path_for_output(path, overwrite = overwrite)
        utils::zip(path, files = files, extras = "-j")
        unlink(files)
      }

    # spatial raster class
    } else if (inherits(ds, c("SpatRaster", "PackedSpatRaster"))) {

      # write TIFF file
      if ("tiff" %in% formats) {
        path <- sprintf("%s/%s.tiff", destdir, name)
        checkmate::assert_path_for_output(path, overwrite = overwrite)
        terra::unwrap(ds) |>
          terra::writeRaster(filename = path, overwrite = TRUE, verbose = !quiet)
      }

    # data frame class
    } else if (inherits(ds, "data.frame")) {

      # write CSV file
      if ("csv" %in% formats) {
        path <- sprintf("%s/%s.csv", destdir, name)
        checkmate::assert_path_for_output(path, overwrite = overwrite)
        utils::write.table(ds,
          file = path,
          sep = ",",
          row.names = FALSE,
          qmethod = "double",
          fileEncoding = "UTF-16LE"
        )
      }

      # write JSON file
      if ("json" %in% formats) {
        path <- sprintf("%s/%s.json", destdir, name)
        checkmate::assert_path_for_output(path, overwrite = overwrite)
        jsonlite::toJSON(ds, null = "null", na = "null", digits = 8, pretty = pretty) |>
          writeLines(con = path, useBytes = TRUE)
      }

      # write Parquet file
      if ("parquet" %in% formats) {
        path <- sprintf("%s/%s.parquet", destdir, name)
        checkmate::assert_path_for_output(path, overwrite = overwrite)
        arrow::write_parquet(ds, sink = path, compression = "snappy")
      }

      # write XLSX file
      if ("xlsx" %in% formats) {
        path <- sprintf("%s/%s.xlsx", destdir, name)
        checkmate::assert_path_for_output(path, overwrite = overwrite)
        writexl::write_xlsx(ds, path = path)
      }

    # coordinate reference system class
    } else if (inherits(ds, "crs")) {

      # write TXT file
      if ("txt" %in% formats) {
        path <- sprintf("%s/%s.txt", destdir, name)
        checkmate::assert_path_for_output(path, overwrite = overwrite)
        writeLines(ds$wkt, con = path)
      }

    # other classes
    } else {
      sprintf("Class '%s' for dataset '%s' is not accounted for.", class(ds), name) |>
        stop(call. = FALSE)
    }

    # append path
    paths <- c(paths, path)
  }

  # return output paths
  invisible(paths)
}

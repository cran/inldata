#' Manage Cache Directory
#'
#' @description Find, create, or clear the cache directory.
#'   Defaults to the temporary directory if the \pkg{rappdirs} package is unavailable.
#'   You can specify the path to the cache directory by setting an environment variable named "CACHE_DIR".
#'
#' @param name 'character' string.
#'   Name of cache directory.
#'
#' @return Path to the cache directory.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' get_cache_dir("test")
#'
#' clear_cache_dir("test")

get_cache_dir <- function(name = "inldata") {
  checkmate::assert_string(name)

  dir <- Sys.getenv("CACHE_DIR")

  if (!checkmate::test_directory_exists(dir, access = "rw")) {
    if (requireNamespace("rappdirs", quietly = TRUE)) {
      dir <- rappdirs::user_cache_dir(name) |>
        normalizePath(winslash = "/", mustWork = FALSE)
      dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    } else {
      dir <- tempdir()
    }
  }

  dir
}

#' @rdname get_cache_dir
#' @export
#' @keywords internal

clear_cache_dir <- function(name = "inldata") {
  checkmate::assert_string(name)
  dir <- get_cache_dir(name)
  is <- checkmate::test_directory(dir, access = "w")
  if (is) {
    unlink(dir, recursive = TRUE)
  }
  invisible(NULL)
}

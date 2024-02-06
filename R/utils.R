#' Parse Station Names
#'
#' @description Convert station names to common site names.
#'
#' @param x 'character' vector.
#'   Station names, such as `03N 29E 01DBB1    USGS 98`.
#'
#' @return A vector of common site names parsed from `x`.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' x <- c(
#'   "02N 30E 08DAD1               BADGING FACILITY",
#'   "03N 29E 33CCC1 MIDDLE 2051 PORT6 ZONE6 826.8FT",
#'   "MUD LAKE NR TERRETON ID"
#' )
#' inldata::parse_station_nm(x)

parse_station_nm <- function(x) {
  checkmate::assert_character(x, any.missing = FALSE)
  is <- grepl(
    pattern = "^[0-9]+[A-Za-z]\\s[0-9]+[A-Za-z]\\s[A-Za-z0-9]+\\s",
    x = x
  )
  x[is] <- substr(x[is],
    start = 15,
    stop = nchar(x[is])
  )
  is <- grepl(" PORT", x) & grepl(" ZONE", x)
  x[is] <- substr(x[is],
    start = 1,
    stop = regexpr(" PORT", x[is]) - 1L
  )
  trimws(x)
}


#' Trim Station Names
#'
#' @description Convert station names to common site names.
#'
#' @param x 'character' vector.
#'   Station names, such as `03N 29E 01DBB1    USGS 98`.
#'
#' @return A vector of common site names parsed from `x`.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' x <- c(
#'   "03N 29E 12DDB1    FIRE STA 2",
#'   "03N 29E 24CCA1 MIDDLE 2050A PORT15 ZONE15 516.8FT"
#' )
#' inldata::trim_station_nm(x)

trim_station_nm <- function(x) {
  checkmate::assert_character(x, any.missing = FALSE)
  strsplit(x, split = "  ") |>
    vapply(
      FUN = function(e) {
        e[nchar(e) > 0] |> trimws() |> paste(collapse = " ")
      },
      FUN.VALUE = character(1)
    )
}


#' Round Numbers
#'
#' @description Rounds the values in its first argument to the specified number of decimal places.
#'   This function uses the U.S. Geological Survey rounding method.
#'
#' @param x 'numeric' vector.
#'   Value to be rounded.
#' @param digits 'integer' vector or value.
#'   Number of decimal places to use (default 0).
#'   Values are recycled to match the vector length of `x`.
#'
#' @return A numeric vector of rounded values.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' inldata::round_usgs(x = rep(pi, 3), digits = c(1, 2, 3))

round_usgs <- function(x, digits = 0) {
  checkmate::assert_numeric(x)
  checkmate::assert_integerish(digits, max.len = length(x))
  z <- abs(x) * 10^digits + 0.5 + sqrt(.Machine$double.eps)
  trunc(z) / 10^digits * sign(x)
}


#' Concatenate Character Vectors
#'
#' @description Concatenate character vectors and omit empty strings.
#'
#' @param ... 'character' vectors (or objects coercible to character vectors).
#'   Corresponding elements are to be concatenated.
#' @param collapse 'character' string.
#'   Seperates the results.
#' @param recycle0 'logical' flag.
#'   Whether a zero-length character argument should lead to a
#'   zero-length character (`character(0)`) being returned.
#'
#' @return Returns a character vector.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' inldata::paste_strings(letters, c(), c(1, 2, 3))
#'
#' inldata::paste_strings(letters, c(), c(1, 2, 3), recycle0 = TRUE)

paste_strings <- function(..., collapse = " ", recycle0 = FALSE) {
  check_package(pkg = "stringi", msg = "Concatenating character vectors")
  checkmate::assert_string(collapse)
  checkmate::assert_logical(recycle0)
  dots <- list(...) |> lapply(FUN = as.character)
  lens <- lengths(dots)
  if (recycle0 && 0L %in% lens) {
    return(character(0))
  }
  lapply(dots, rep_len, length.out = max(lens)) |>
    as.data.frame() |>
    apply(
      MARGIN = 1,
      FUN = stringi::stri_flatten,
      collapse = collapse,
      na_empty = TRUE,
      omit_empty = TRUE
    )
}


#' Check Package
#'
#' @description Check that a package is available.
#'
#' @param pkg 'character' string.
#'   Package name.
#' @param msg 'character' string.
#'   Action package is used for.
#' @param call. 'logical' flag.
#'   Whether the function should be part of the error message.
#'
#' @return Stops execution if the package is missing from the name space.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' inldata::check_package(pkg = "inldata", msg = "Dataset access")

check_package <- function(pkg, msg = NULL, call. = FALSE) {
  checkmate::assert_string(pkg)
  checkmate::assert_string(msg, null.ok = TRUE)
  checkmate::assert_flag(call.)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    s <- sprintf("requires the '%s' package", pkg)
    if (!is.null(msg)) {
      s <- paste(msg, s)
    }
    stop(s, call. = call.)
  }
}

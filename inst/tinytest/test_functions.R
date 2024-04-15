# setup tinytest for checkmate functionality
library("tinytest")
library("checkmate")
using("checkmate")

# test parsing of station name
x <- parse_station_nm("03N 29E 33CCC1 MIDDLE 2051 PORT6 ZONE6 826.8FT")
expect_set_equal(x, "MIDDLE 2051")

# test parsing of help documentation
l <- parse_rd_db(package = "inldata")
expect_list(l, types = "list", names = "named")

# test metadata validation
if (test_file_exists(Sys.which("mp"), access = "x")) {
  dir <- tempfile("")
  x <- validate_metadata(
    file = system.file("extdata/test.xml", package = "inldata"),
    destdir = dir,
    opts = NULL
  )
  expect_false(x)
  unlink(dir, recursive = TRUE)
}

# test URL assertion
expect_silent(assert_url("https://www.usa.gov/"))
expect_error(assert_url("https://fail/on/bad/url/"))

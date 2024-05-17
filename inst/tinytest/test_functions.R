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
expect_error(assert_url("https://anyapi.io/api/v1/exchange/rates?base=NAN&apiKey=123"))

# test downloading file
url <- "https://code.usgs.gov/inl/inldata/-/raw/main/CODE_OF_CONDUCT.md"
file <- download_file(url, cachedir = tempdir(), quiet = TRUE)
expect_file_exists(file, access = "rw")
unlink(file)

# test cleaning simple feature
x <- clean_sf(cities,
  cols = c("key" = "id", "geometry" = "geometry"),
  agr = c("key" = "identity"),
  crs = sf::st_crs(3857),
  extent = sf::st_bbox(esrp)
)
expect_multi_class(x, classes = c("sf", "data.frame"))

#' Create Data Model
#'
#' @description Create a data model object from \pkg{inldata} package datasets.
#'   A data model holds a list of tables and their relationships.
#'   Requires that the \pkg{dm} package is available.
#'
#' @return Invisibly returns the data model, an object of class [`dm`][dm::dm].
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @seealso [`make_erd`] function for creating an entity-relationship diagram.
#'
#' @examples
#' make_dm()

make_dm <- function() {

  # check packages
  check_package(pkg = "dm", msg = "Creating a data model")

  # combine tables into a single list
  combine_tables(package = "inldata") |>

    # convert list to data model
    dm::as_dm() |>

    # add primary keys
    dm::dm_add_pk(table = "background", columns = "pcode", check = TRUE) |>
    dm::dm_add_pk(table = "benchmarks", columns = "pcode", check = TRUE) |>
    dm::dm_add_pk(table = "cities", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "counties", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "dl", columns = c("pcode", "sdate"), check = TRUE) |>
    dm::dm_add_pk(table = "facilities", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "gwl", columns = c("site_no", "lev_dt", "lev_meth_cd"), check = TRUE) |>
    dm::dm_add_pk(table = "lakes", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "parameters", columns = "pcode", check = TRUE) |>
    dm::dm_add_pk(table = "roads", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "samples", columns = c("sample_id", "pcode", "medium_cd"), check = TRUE) |>
    dm::dm_add_pk(table = "sites", columns = "site_no", check = TRUE) |>
    dm::dm_add_pk(table = "streams", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "swm", columns = c("site_no", "stage_dt"), check = TRUE) |>
    dm::dm_add_pk(table = "units", columns = "parm_unit", check = TRUE) |>

    # add secondary keys
    dm::dm_add_fk(table = "background", columns = "pcode", ref_table = "parameters", check = TRUE) |>
    dm::dm_add_fk(table = "background", columns = "parm_unit", ref_table = "units", check = TRUE) |>
    dm::dm_add_fk(table = "benchmarks", columns = "pcode", ref_table = "parameters", check = TRUE) |>
    dm::dm_add_fk(table = "benchmarks", columns = "parm_unit", ref_table = "units", check = TRUE) |>
    dm::dm_add_fk(table = "dl", columns = "pcode", ref_table = "parameters", check = TRUE) |>
    dm::dm_add_fk(table = "dl", columns = "parm_unit", ref_table = "units", check = TRUE) |>
    dm::dm_add_fk(table = "gwl", columns = "site_no", ref_table = "sites", check = TRUE) |>
    dm::dm_add_fk(table = "parameters", columns = "parm_unit", ref_table = "units", check = TRUE) |>
    dm::dm_add_fk(table = "percponds", columns = "facility_id", ref_table = "facilities", check = TRUE) |>
    dm::dm_add_fk(table = "samples", columns = "site_no", ref_table = "sites", check = TRUE) |>
    dm::dm_add_fk(table = "samples", columns = "pcode", ref_table = "parameters", check = TRUE) |>
    dm::dm_add_fk(table = "samples", columns = "parm_unit", ref_table = "units", check = TRUE) |>
    dm::dm_add_fk(table = "swm", columns = "site_no", ref_table = "sites", check = TRUE) |>

    # validate data model
    dm::dm_validate()
}


# Funtion that combines all package table datasets into a single list ----

combine_tables <- function(package) {

  # check arguments
  checkmate::assert_string(package)

  # combine tables sourced from Rdata files
  if (checkmate::test_file_exists("DESCRIPTION", access = "r")) {
    pkg <- read.dcf("DESCRIPTION", fields = "Package") |> as.character()
    if (pkg == package) {
      files <- list.files("data", pattern = "*.rda", full.names = TRUE)
      if (length(files) > 0) {
        ds_names <- basename(files) |> tools::file_path_sans_ext()
        l <- lapply(files,
          function(x) {
            env <- environment()
            nm <- load(file = x, envir = env)
            tryCatch(
              {
                env[[nm]] |> as.data.frame()
              },
              error = function(e) {
                return(NULL)
              }
            )
          }
        )
        names(l) <- ds_names
        return(ccp(l))
      }
    }
  }

  # combine tables sourced from package datasets
  ds_names <- utils::data(package = package)$results[, "Item"]
  l <- lapply(ds_names,
    function(x) {
      env <- environment()
      nm <- utils::data(list = x, package = package, envir = env)[1]
      tryCatch(
        {
          env[[nm]] |> as.data.frame()
        },
        error = function(e) {
          return(NULL)
        }
      )
    }
  )
  names(l) <- ds_names
  ccp(l)
}


# Function that removes elements from a list that are equal to NULL ----

ccp <- function(x) {
  checkmate::assert_vector(x, null.ok = TRUE)
  Filter(Negate(is.null), x)
}

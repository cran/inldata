#' Create Package Datasets
#'
#' @description Create datasets for the \pkg{inldata} package and save each as an
#'   R-data file with the `.rda` extension, which is a format native to \R.
#'   The \pkg{stats} \pkg{dataRetrieval}, and \pkg{stringi} packages must be available,
#'   and the [7z](https://www.7-zip.org/) executable must be on your path.
#'   This function is intended for use by \pkg{inldata}-package developers.
#'
#' @param path 'character' string.
#'   Path to the package's source directory, with tilde-expansion performed.
#'   Defaults to the working directory.
#'   Ensure that under the `path` is a folder named `data-raw`
#'   that contains the raw data files required for the build process.
#' @param destdir 'character' string.
#'   Destination directory to write R-data files, with tilde-expansion performed.
#'   Defaults to the `data` directory located under `path`.
#' @param clean 'logical' flag.
#'   Whether to delete all pre-existing R-data files in the destination directory.
#' @param tz 'character' string.
#'   [Time zone][base::timezones] specification.
#'   Defaults to Mountain Standard Time (North America).
#'   See [`OlsonNames`] for time zone information.
#' @param buffer_dist 'numeric' number.
#'   Buffer distance for the study area defined by the bounding of the sample [`sites`] dataset.
#'   The buffer distance is measured in units of the coordinate reference system
#'   ([`crs$units`][sf::st_crs]).
#' @param warn 'integer' value.
#'   Sets the handling of warning messages.
#'   Choose value of less than 0 to show no warnings, 1 to print warnings (default),
#'   and 2 to error on warnings.
#' @param timeout 'integer' number.
#'  Timeout for some of the internet operations, in minutes.
#'  Defaults to 10 minutes.
#' @param compress 'logical' flag or 'character' string.
#'   Whether compression should be used when saving a dataset to file.
#'   Character strings "auto", "gzip", "bzip2" and "xz" (default) are accepted.
#'   See the [`save`] function for details on compression types.
#'
#' @details This function retrieves and parses datasets from local and remote sources.
#'   Access to the internet is required to download data from the following remote sources:
#'   - National Elevation Dataset ([NED](https://www.usgs.gov/publications/national-elevation-dataset))
#'     on [Amazon's Cloud](https://prd-tnm.s3.amazonaws.com/).
#'   - Spatial data from the
#'     [TIGER/Line Geodatabase](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-geodatabase-file.html)
#'     that contains spatial extracts from the U.S. Census Bureau's
#'     [MAF/TIGER database](https://www2.census.gov/geo/tiger/TGRGDB22/).
#'   - National Hydrography Dataset
#'     ([NHD](https://www.usgs.gov/national-hydrography/national-hydrography-dataset))
#'     data from the USGS NHD file geodatabase on [Amazon's Cloud](https://dmap-data-commons-ow.s3.amazonaws.com/).
#' @details Each of the package dataset's represents a snapshot of the data at a specified point in time.
#'   While geospatial datasets may change very little over time
#'   (such as the boundary of the Idaho National Laboratory),
#'   other datasets continue to grow as new data becomes available
#'   (such as water-quality data measured in [`samples`] collected from wells).
#' @details To ensure that the function retrieves the most recent data versions,
#'   it is recommended to periodically check the URLs of remote sources and update them within the function.
#'   It is also advisable to document any changes in the datasets and update their help documentation accordingly.
#' @details Files downloaded during intermediate stages of the build process
#'   are cached on your computer to speed up future builds.
#'   You can specify the path to the cache directory by setting an environment variable named `CACHE_DIR`.
#'   By default the location of the cache directory is determined by the [`get_cache_dir()`] command.
#'
#' @return Returns the paths to the newly created R Data files invisibly.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' # Example requires that the path be set to the inldata package source directory.
#' \dontrun{
#' make_datasets(destdir = tempfile(""))
#' }

make_datasets <- function(path = getwd(),
                          destdir = file.path(path, "data"),
                          clean = FALSE,
                          tz = "America/Denver",
                          buffer_dist = 1000,
                          warn = 1,
                          timeout = 10,
                          compress = "xz") {

  # track computation time
  dt <- Sys.time()
  message("TIMESTAMP: ", format(dt, usetz = TRUE))

  # check arguments
  path <- path.expand(path) |> normalizePath(winslash = "/", mustWork = FALSE)
  checkmate::assert_directory_exists(path, access = "rw")
  sprintf("%s/data-raw", path) |> checkmate::assert_directory_exists(access = "r")
  destdir <- path.expand(destdir) |> normalizePath(winslash = "/", mustWork = FALSE)
  checkmate::assert_flag(clean)
  checkmate::assert_choice(tz, choices = OlsonNames())
  checkmate::assert_int(warn)
  checkmate::assert_number(timeout, lower = 1, finite = TRUE)
  if (!is.logical(compress)) {
    checkmate::assert_choice(compress, choices = c("auto", "gzip", "bzip2", "xz"))
  }

  # check packages
  for (pkg in c("dataRetrieval", "stats", "stringi")) {
    check_package(pkg, msg = "Making datasets")
  }

  # check system dependencies
  Sys.which("7z") |> checkmate::assert_file_exists(access = "x")
  if (!capabilities("libcurl")) {
    stop("libcurl is unavailable", call. = FALSE)
  }

  # set global options
  op <- options(warn = warn, timeout = timeout * 60, useFancyQuotes = FALSE)
  on.exit(options(op))

  # make temporary directory
  tmpdir <- tempfile("")
  dir.create(tmpdir, showWarnings = FALSE)

  # get cache directory
  cachedir <- get_cache_dir()

  # make coordinate reference system dataset (crs)
  message("STATUS: making 'crs' dataset ...")
  file <- file.path(path, "data-raw/misc/projection.txt")
  crs <- mds_crs(file)
  save(crs, file = file.path(tmpdir, "crs.rda"), compress = FALSE)

  # make laboratory detection limits dataset (dl)
  message("STATUS: making 'dl' dataset ...")
  file <- file.path(path, "data-raw/misc/detection-limits.tsv")
  dl <- mds_dl(file)
  save(dl, file = file.path(tmpdir, "dl.rda"), compress = FALSE)

  # make parameter information for analytes dataset (parameters)
  message("STATUS: making 'parameters' dataset ...")
  file <- file.path(path, "data-raw/qwdata/pcodes.txt")
  parameters <- mds_parameters(file)
  save(parameters, file = file.path(tmpdir, "parameters.rda"), compress = FALSE)

  # make water-quality samples dataset (samples)
  message("STATUS: making 'samples' dataset ...")
  files <- file.path(path, c(
    "data-raw/qwdata/output.rdb",
    "data-raw/misc/translate-codes.tsv",
    "data-raw/misc/counting-error.tsv"
  ))
  samples <- mds_samples(files, tz, dl, parameters)
  save(samples, file = file.path(tmpdir, "samples.rda"), compress = FALSE)

  # make benchmark concentrations dataset (benchmarks)
  message("STATUS: making 'benchmarks' dataset ...")
  files <- file.path(path, c(
    "data-raw/misc/benchmarks.csv",
    "data-raw/misc/benchmarks-extras.tsv"
  ))
  benchmarks <- mds_benchmarks(files, parameters)
  save(benchmarks, file = file.path(tmpdir, "benchmarks.rda"), compress = FALSE)

  # make site information dataset (sites)
  message("STATUS: making 'sites' dataset ...")
  file <- file.path(path, "data-raw/qwdata/siteids.txt")
  sites <- mds_sites(file, crs)
  save(sites, file = file.path(tmpdir, "sites.rda"), compress = FALSE)

  # set spatial extent
  sp_extent <- sf::st_buffer(sites, dist = buffer_dist) |> sf::st_bbox()

  # make surface-water measurements dataset (swm)
  message("STATUS: making 'swm' dataset ...")
  swm <- mds_swm(tz, sites)
  save(swm, file = file.path(tmpdir, "swm.rda"), compress = FALSE)

  # make groundwater levels dataset (gwl)
  message("STATUS: making 'gwl' dataset ...")
  gwl <- mds_gwl(tz, sites)
  save(gwl, file = file.path(tmpdir, "gwl.rda"), compress = FALSE)

  # make parameter units dataset (units)
  message("STATUS: making 'units' dataset ...")
  file <- file.path(path, "data-raw/misc/units.tsv")
  units <- mds_units(file)
  save(units, file = file.path(tmpdir, "units.rda"), compress = FALSE)

  # make background concentrations dataset (background)
  message("STATUS: making 'background' dataset ...")
  file <- file.path(path, "data-raw/misc/background.tsv")
  background <- mds_background(file)
  save(background, file = file.path(tmpdir, "background.rda"), compress = FALSE)

  # make eastern Snake River Plain boundary dataset (esrp)
  message("STATUS: making 'esrp' dataset ...")
  file <- file.path(path, "data-raw/misc/esrp.geojson")
  esrp <- mds_esrp(file, crs)
  save(esrp, file = file.path(tmpdir, "esrp.rda"), compress = FALSE)

  # make Idaho National Laboratory boundary dataset (inl)
  message("STATUS: making 'inl' dataset ...")
  file <- file.path(path, "data-raw/misc/inl.geojson")
  inl <- mds_inl(file, crs)
  save(inl, file = file.path(tmpdir, "inl.rda"), compress = FALSE)

  # make industrial waste ditch dataset (iwd)
  message("STATUS: making 'iwd' dataset ...")
  file <- file.path(path, "data-raw/misc/nrfiwd.geojson")
  iwd <- mds_iwd(file, crs)
  save(iwd, file = file.path(tmpdir, "iwd.rda"), compress = FALSE)

  # make Idaho National Laboratory facilities dataset (facilities)
  message("STATUS: making 'facilities' dataset ...")
  file <- file.path(path, "data-raw/misc/facilities.geojson")
  facilities <- mds_facilities(file, crs)
  save(facilities, file = file.path(tmpdir, "facilities.rda"), compress = FALSE)

  # make percolation ponds dataset (percponds)
  message("STATUS: making 'percponds' dataset ...")
  file <- file.path(path, "data-raw/misc/percponds.geojson")
  percponds <- mds_percponds(file, crs)
  save(percponds, file = file.path(tmpdir, "percponds.rda"), compress = FALSE)

  # make mountain ranges and buttes dataset (mountains)
  message("STATUS: making 'mountains' dataset ...")
  file <- file.path(path, "data-raw/misc/mountains.geojson")
  mountains <- mds_mountains(file, crs, sp_extent)
  save(mountains, file = file.path(tmpdir, "mountains.rda"), compress = FALSE)

  # make state of Idaho boundary dataset (idaho)
  message("STATUS: making 'idaho' dataset ...")
  url <- "ftp://ftp2.census.gov/geo/tiger/TIGER2022/STATE/tl_2022_us_state.zip"
  idaho <- mds_idaho(url, cachedir, crs)
  save(idaho, file = file.path(tmpdir, "idaho.rda"), compress = FALSE)

  # make cities and towns dataset (cities)
  message("STATUS: making 'cities' dataset ...")
  url <- "ftp://ftp2.census.gov/geo/tiger/TIGER2022/PLACE/tl_2022_16_place.zip"
  cities <- mds_cities(url, cachedir, crs, sp_extent)
  save(cities, file = file.path(tmpdir, "cities.rda"), compress = FALSE)

  # make county boundaries dataset (counties)
  message("STATUS: making 'counties' dataset ...")
  url <- "ftp://ftp2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip"
  counties <- mds_counties(url, cachedir, crs, sp_extent)
  save(counties, file = file.path(tmpdir, "counties.rda"), compress = FALSE)

  # make road netowrk dataset (roads)
  message("STATUS: making 'roads' dataset ...")
  urls <- c(
    "ftp://ftp2.census.gov/geo/tiger/TIGER2022/PRISECROADS/tl_2022_16_prisecroads.zip",
    "ftp://ftp2.census.gov/geo/tiger/TIGER2022/ROADS/tl_2022_16023_roads.zip"
  )
  roads <- mds_roads(urls, cachedir, crs, sp_extent)
  save(roads, file = file.path(tmpdir, "roads.rda"), compress = FALSE)

  # make lakes and ponds dataset (lakes)
  message("STATUS: making 'lakes' dataset ...")
  url <- "https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/NHDPlusPN/NHDPlusV21_PN_17_NHDSnapshot_08.7z"
  lakes <- mds_lakes(url, cachedir, crs, sp_extent)
  save(lakes, file = file.path(tmpdir, "lakes.rda"), compress = FALSE)

  # make rivers and streams dataset (streams)
  message("STATUS: making 'streams' dataset ...")
  url <- "https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/NHDPlusPN/NHDPlusV21_PN_17_NHDSnapshot_08.7z"
  streams <- mds_streams(url, cachedir, crs, sp_extent)
  save(streams, file = file.path(tmpdir, "streams.rda"), compress = FALSE)

  # make digital elevation model dataset (dem)
  message("STATUS: making 'dem' dataset ...")
  ids <- c("n44w113", "n44w114", "n45w113", "n45w114")
  urls <- sprintf(
    fmt = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/current/%s/USGS_13_%s.tif",
    ids, ids
  )
  dem <- mds_dem(urls, cachedir, crs, sp_extent)
  save(dem, file = file.path(tmpdir, "dem.rda"), compress = FALSE)

  # compress temporary files
  message("STATUS: compress dataset files ...")
  if (is.character(compress) || compress) {
    tools::resaveRdaFiles(paths = tmpdir, compress = compress, version = 3L)
  }

  # copy temporary files to destination directory
  message("STATUS: copy dataset files to destination directory ...")
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
  if (clean) {
    sprintf("%s/*.rda", destdir) |> unlink()
  }
  files <- list.files(path = tmpdir, full.names = TRUE)
  file.copy(from = files, to = destdir, overwrite = TRUE)
  unlink(tmpdir, recursive = TRUE)

  # get paths to files in destination directory
  paths <- file.path(destdir, basename(files), fsep = "/")

  # print computation time
  message("DURATION: ", format(Sys.time() - dt, usetz = TRUE))

  invisible(paths)
}


# Coordinate Reference System (crs) ----

mds_crs <- function(file) {
  checkmate::assert_file_exists(file, access = "r")
  x <- readLines(con = file, encoding = "UTF-8")
  sf::st_crs(x)
}


# Laboratory Detection Limits (dl) -----

mds_dl <- function(file) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")

  # read file
  d <- utils::read.delim(file, comment.char = "#", colClasses = "character")

  # convert column class
  d$parm_unit <- as.factor(d$parm_unit)
  d$lab_det_lim_va <- as.numeric(d$lab_det_lim_va)
  d$sdate <- as.Date(d$sdate)
  d$reference <- as.factor(d$reference)

  # order and remove row names
  idxs <- tolower(d$srsname) |> stringi::stri_order(numeric = TRUE)
  d <- d[idxs, ]
  rownames(d) <- NULL

  d
}


# Parameter Information for Analytes (parameters) -----

mds_parameters <- function(file) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")

  # read metadata file
  meta <- utils::read.delim(file = file, header = FALSE, colClasses = "character")
  colnames(meta) <- c("pcode", "parm_nm")

  # check for duplicated values
  if (any(is <- duplicated(meta$pcode))) {
    txt <- sQuote(meta$pcode[is]) |> paste(collapse = ", ")
    stop("Duplicated parameter codes: ", txt, call. = FALSE)
  }

  # download data
  d <- dataRetrieval::readNWISpCode(meta$pcode)

  cols <- c(
    "parm_nm" = "parameter_nm",
    "pcode" = "parameter_cd",
    "parm_group_nm" = "parameter_group_nm",
    "casrn" = "casrn",
    "srsname" = "srsname",
    "parm_unit" = "parameter_units"
  )
  d <- d[, cols]
  colnames(d) <- names(cols)

  d$casrn <- trimws(d$casrn)
  d$casrn[d$casrn == ""] <- NA_character_

  d$srsname <- trimws(d$srsname)
  d$srsname[d$srsname == "Trihalomethanes (four), total, from SDWA NPDWR"] <- "TTHM4"
  d$srsname[d$srsname == ""] <- NA_character_

  d$parm_unit <- trimws(d$parm_unit)
  subs <- c(
    "mg/L" = "^mg/l$",
    "mg/L as CaCO3" = "^mg/l CaCO3$",
    "mg/L " = "^mg/l ",
    "ug/L" = "^ug/l$",
    "ug/L " = "^ug/l "
  )
  for (i in seq_along(subs)) {
    d$parm_unit <- sub(pattern = subs[i], replacement = names(subs)[i], x = d$parm_unit)
  }
  d$parm_unit[d$parm_unit == ""] <- NA_character_

  # convert classes
  d$parm_group_nm <- as.factor(d$parm_group_nm)
  d$parm_unit <- as.factor(d$parm_unit)

  idxs <- tolower(d$parm_nm) |> stringi::stri_order(numeric = TRUE)
  d <- d[idxs, ]
  rownames(d) <- NULL

  d
}


# Water-Quality Data Records (samples) -----

mds_samples <- function(files, tz, dl, parameters) {

  # check arguments
  checkmate::assert_character(files, len = 3, any.missing = FALSE)
  for (file in files) checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_string(tz)
  checkmate::assert_data_frame(dl, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(parameters, min.rows = 1, col.names = "named")

  # read sample records file
  v <- readLines(files[1])
  i <- 0L
  repeat {
    ss <- v[i + 1L] |> substr(start = 1, stop = 1)
    if (ss != "#") break
    i <- i + 1L
  }
  d <- utils::read.table(
    file = files[1],
    sep = "\t",
    quote = "",
    na.strings = "NA",
    skip = i + 2L,
    strip.white = TRUE,
    colClasses = "character"
  )
  hdr <- v[i + 1L] |> strsplit(split = "\t") |> unlist()
  colnames(d) <- hdr
  remark <- v[seq_len(i)]

  # read translate codes file
  codes <- utils::read.delim(file = files[2], colClasses = "character")
  dic <- codes$nwis_cd
  names(dic) <- codes$alpha_cd
  colnames(d) <- dic[hdr]
  idxs <- which(dic[hdr] == "")
  if (length(idxs) > 0) {
    txt <- hdr[idxs] |> sQuote() |> paste(collapse = ", ")
    warning("Missing database names for ALPHA codes:\n  ", txt, call. = FALSE, immediate. = TRUE)
    d <- d[, -idxs]
  }

  # set result units
  idxs <- match(d$pcode, parameters$pcode)
  if (any(is <- is.na(idxs))) {
    txt <- sQuote(d$pcode[idxs][is]) |> paste(collapse = ", ")
    stop("Samples contains unrecognized parameter codes: ", txt, call. = FALSE)
  }
  d$parm_unit <- parameters$parm_unit[idxs]

  # set site name
  site_nm <- parse_station_nm(d$station_nm)
  d <- data.frame(site_nm, d)

  # account for missing time
  d$sample_tm[d$sample_tm == ""] <- "1200"
  d$sample_dt <- paste(d$sample_dt, d$sample_tm) |>
    as.POSIXct(tz = tz, format = "%Y%m%d %H%M")
  d$sample_tm <- NULL

  # get the number of digits to the right of the decimal point in a number
  d$result_scale_va <- sub("\\d+\\.?(.*)$", "\\1", d$result_va) |> nchar()

  d$result_va <- as.numeric(d$result_va)
  d$anl_stat_cd <- NULL

  for (i in grep("_va$", colnames(d))) {
    d[[i]] <- as.numeric(d[[i]])
  }

  # remove unnecessary columns
  cols <- c("station_nm", "parm_nm")
  idxs <- match(cols, colnames(d)) |> stats::na.omit()
  if (length(idxs) > 0) {
    d <- d[, -idxs]
  }

  # remove records that are duplicated in the NWIS and QWDATA databases
  idxs <- which(colnames(d) %in% c("db_no", "dqi_cd"))
  is <- d[, -idxs] |> duplicated()
  d <- d[!is, ]

  # remove contaminated results
  d <- d[d$remark_cd != "V", ]

  # initialize remark
  d$remark <- nrow(d) |> character()

  # report zero and negative results as nondetects
  is <- d$remark_cd == "<" & !is.na(d$result_va) & d$result_va <= 0
  d$remark_cd[is] <- character(1)
  txt <- "Change remark code from '<' (nondetect) to '' because result value is less than or equal to 0"
  d$remark[is] <- paste_strings(d$remark[is], txt, collapse = "; ", recycle0 = TRUE)

  # identify radiochemical parameter codes
  is <- parameters$pcode %in% d$pcode & parameters$parm_group_nm %in% "Radiochemical"
  radchem_pcode <- parameters$pcode[is]

  # report radiochemical nondetects as less than the reporting level
  is <- d$remark_cd == "R" & d$pcode %in% radchem_pcode
  d$result_va[is] <- d$rpt_lev_va[is]
  d$remark_cd[is] <- "<"
  txt <- "Substitute result value with reporting level value and change remark code from 'R' to '<'"
  d$remark[is] <- paste_strings(d$remark[is], txt, collapse = "; ", recycle0 = TRUE)

  # check sample type codes
  choices <- c(
    "blank" = "2",
    "reference material" = "6",
    "replicate" = "7",
    "regular" = "9",
    "not determined" = "A",
    "other QA" = "B",
    "composite (time)" = "H"
  )
  unique(d$sample_type_cd) |>
    checkmate::assert_subset(choices = choices, empty.ok = FALSE)

  # assume unrecorded sample types are regular environmental samples
  is <- d$sample_type_cd == "A"
  d$sample_type_cd[is] <- "9"
  txt <- "Change sample type code from 'A' (not recorded) to '9' (regular sample)"
  d$remark[is] <- paste_strings(d$remark[is], txt, collapse = "; ", recycle0 = TRUE)

  # add column that identifies a unique sample
  d$sample_id <- paste0(
    d$site_no,
    d$medium_cd,
    format(d$sample_dt, format = "%Y%m%d%H%M", tz = "GMT")
  )

  # read counting error file
  counting_error <- utils::read.delim(file = files[3], colClasses = "character")

  # convert counting error to standard deviaiton
  is_lt_1989 <- d$sample_dt < as.POSIXct("2008-01-01")
  for (i in seq_len(nrow(counting_error))) {
    cd <- counting_error$pcode[i]
    ce <- counting_error$pcode_ce[i]

    is_cd <- d$pcode == cd & is.na(d$lab_sd_va)
    is_ce <- d$pcode == ce

    d0 <- d[is_cd, "sample_id", drop = FALSE]
    d0$idx <- rownames(d0) |> as.integer()
    d1 <- d[is_ce, c("sample_id", "result_va")]
    d0 <- merge(d0, d1, by = "sample_id", all.x = TRUE)
    d$lab_sd_va[is_cd] <- d0[order(d0$idx), "result_va"]

    txt <- "Determine laboratory standard deviation from counting error"
    d$remark[is_cd] <- paste_strings(d$remark[is_cd], txt, collapse = "; ", recycle0 = TRUE)

    # workaround for data entered into NWIS with uncertainty of 2s
    is <- is_cd & is_lt_1989
    d$lab_sd_va[is] <- d$lab_sd_va[is] / 2

    txt <- "Divide laboratory standard deviation by 2 because sample collected before Jan 1, 2008"
    d$remark[is] <- paste_strings(d$remark[is], txt, collapse = "; ", recycle0 = TRUE)
  }

  # set identifier for replicate-sample pairs
  d$id <- seq_len(nrow(d))

  # subset replicate samples
  env_samp <- d[d$sample_type_cd == "9", ]
  rep_samp <- d[d$sample_type_cd == "7", ]

  # convert to day
  env_day <- as.Date(env_samp$sample_dt) |> as.integer()
  rep_day <- as.Date(rep_samp$sample_dt) |> as.integer()

  # convert to numeric in seconds
  env_sec <- as.numeric(env_samp$sample_dt)
  rep_sec <- as.numeric(rep_samp$sample_dt)

  # identify replicate pair samples
  ids <- vapply(seq_along(rep_sec), function(i) {
    idxs <- {
      env_samp$site_no == rep_samp$site_no[i] & # match site number
      env_samp$pcode == rep_samp$pcode[i] & # match parameter code
      env_day == rep_day[i] # same day
    } |> which()
    if (length(idxs) == 0) {
      return(NA_integer_)
    }
    tdiff <- env_sec[idxs] - rep_sec[i]
    idx <- abs(tdiff) |> which.min()
    if (length(idx) == 0) {
      return(NA_integer_)
    }
    env_samp$id[idxs][idx]
  }, integer(1))

  # warn if replicate record is unpaired
  orphans <- rep_samp[is.na(ids), ]
  ignore_pcode <- c(
    "99111", # type of quality assurance data associated with sample, code
    "99105", # type of replicate, code
    "99102", # type of blank sample, code
    "99101", # source of blank solution, code
    "99100", # type of blank solution, code
    "84164", # sampler type, code
    "82398", # sampling method, code
    "71999" # sample purpose, code
  )
  is <- orphans$pcode %in% ignore_pcode
  orphans <- orphans[!is, ]
  if (nrow(orphans) > 0) {
    txt <- sprintf("  %s %s (%s)", orphans$sample_dt, orphans$site_no, orphans$site_nm) |>
      unique() |> sort()
    sprintf("Unable to pair %d replicate samples:", length(txt)) |>
      warning(call. = FALSE, immediate. = TRUE)
    paste(txt, collapse = "\n") |> message()
  }

  # pair replicate samples
  m <- cbind("env" = ids, "rep" = rep_samp$id)
  m <- m[!is.na(ids), ]
  d$rep_pair_id <- NA_integer_
  d$rep_pair_id[m[, "env"]] <- m[, "env"]
  d$rep_pair_id[m[, "rep"]] <- m[, "env"]
  d$id <- NULL

  # set laboratory detection limits
  d$lab_det_lim_va <- NA_real_
  for (i in seq_len(nrow(dl))) {
    is <- d$pcode == dl$pcode[i] & as.Date(d$sample_dt) >= dl$sdate[i]
    d$lab_det_lim_va[is] <- dl$lab_det_lim_va[i]
  }

  # represent radionuclide result value using confidence interval
  conf <- 0.95
  error <- d$lab_sd_va * stats::qnorm(1 - (1 - conf) / 2)
  error[is.na(error)] <- 0
  li <- d$result_va - error
  ui <- d$result_va + error
  is <- is.finite(li) & is.finite(d$lab_det_lim_va) & li < d$lab_det_lim_va
  li[is] <- 0
  is <- is.finite(ui) & is.finite(d$lab_det_lim_va) & ui < d$lab_det_lim_va
  li[is] <- 0
  ui[is] <- d$lab_det_lim_va[is]
  is <- is.finite(li) & li < 0
  li[is] <- 0
  is <- is.finite(ui) & ui < 0
  li[is] <- 0
  ui[is] <- 0
  li <- round_usgs(li, digits = d$result_scale_va)
  ui <- round_usgs(ui, digits = d$result_scale_va)
  d$lab_li_va <- li
  d$lab_ui_va <- ui

  # represent nondetect result value using confidence interval
  is <- d$remark_cd == "<"
  d$lab_li_va[is] <- 0
  d$lab_ui_va[is] <- d$result_va[is]

  # convert character to factor class
  cols <- c(
    "medium_cd",
    "db_no",
    "remark_cd",
    "dqi_cd",
    "rpt_lev_cd",
    "sample_type_cd",
    "parm_unit"
  )
  for (col in cols) {
    d[[col]] <- as.factor(d[[col]])
  }

  # sort records
  idxs <- order(
    stringi::stri_rank(tolower(d$site_nm), numeric = TRUE),
    stringi::stri_rank(tolower(d$parm_short_nm), numeric = TRUE),
    d$sample_dt
  )
  cols <- c(
    "site_nm",
    "sample_dt",
    "parm_short_nm",
    "parm_unit",
    "remark_cd",
    "result_va",
    "lab_sd_va",
    "lab_li_va",
    "lab_ui_va",
    "rpt_lev_va",
    "rpt_lev_cd",
    "medium_cd",
    "anl_ent_cd",
    "dqi_cd",
    "meth_cd",
    "sample_type_cd",
    "db_no",
    "sample_id",
    "site_no",
    "pcode",
    "rep_pair_id",
    "result_tx",
    "remark"
  )
  d <- d[idxs, cols]
  rownames(d) <- NULL

  d
}


# Benchmark Concentrations (benchmarks) -----

mds_benchmarks <- function(files, parameters) {

  # check arguments
  checkmate::assert_character(files, len = 2, any.missing = FALSE)
  for (file in files) checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_data_frame(parameters, min.rows = 1, col.names = "named")

  # read benchmarks file
  d <- utils::read.csv(
    file = files[1],
    na.strings = c("NA", ""),
    strip.white = TRUE,
    colClasses = "character",
    check.names = FALSE
  )

  cols <- c(
    "srsname" = "Chemical Name",
    "casrn" = "CAS Registry Number",
    "pcode" = "USGS Parameter Code",
    "class" = "Chemical Class",
    "mcl" = "MCL (micrograms/L)",
    "hhbp_noncancer" = "Chronic Noncancer HHBP (micrograms/L)",
    "hhbp_cancer" = "Carcinogenic HHBP (micrograms/L)",
    "hbsl_noncancer" = "Noncancer HBSL (micrograms/L)",
    "hbsl_cancer" = "Cancer HBSL (micrograms/L)",
    "remark" = "Benchmark Remarks"
  )
  d <- d[, cols]
  colnames(d) <- names(cols)

  d$srsname <- NULL
  d$mcl <- as.numeric(d$mcl)
  d$hhbp_noncancer <- as.numeric(d$hhbp_noncancer)
  d$hbsl_noncancer <- as.numeric(d$hbsl_noncancer)
  FUN <- function(x, idx) {
    vapply(strsplit(x, split = "-"), function(y) as.numeric(y[idx]), numeric(1))
  }
  d$hhbp_cancer_min <- FUN(d$hhbp_cancer, 1)
  d$hhbp_cancer_max <- FUN(d$hhbp_cancer, 2)
  d$hbsl_cancer_min <- FUN(d$hbsl_cancer, 1)
  d$hbsl_cancer_max <- FUN(d$hbsl_cancer, 2)

  is <- grepl("^mg/L", parameters$parm_unit)
  is <- d$casrn %in% parameters$casrn[is]
  bm <- c(
    "mcl",
    "hhbp_noncancer",
    "hhbp_cancer_min",
    "hhbp_cancer_max",
    "hbsl_noncancer",
    "hbsl_cancer_min",
    "hbsl_cancer_max"
  )
  d[is, bm] <- d[is, bm] * 0.001

  is <- grepl("^mrem/yr", d$remark)
  d$mcl[is] <- 50 # screening level

  l <- strsplit(d$pcode, split = ", ")
  idxs <- lapply(seq_along(l), function(i) {
    rep(i, length(l[[i]]))
  }) |> unlist()
  d <- d[idxs, ]
  d$pcode <- unlist(l)

  d$parm_unit <- "ug/L"

  p <- parameters[, c("pcode", "srsname")]
  d <- merge(p, d, by = "pcode", all.x = TRUE, sort = FALSE)

  # read maximum contaminant level extras file
  mcl_extras <- utils::read.delim(
    file = files[2],
    strip.white = TRUE,
    colClasses = "character"
  )
  mcl_extras$mcl <- as.numeric(mcl_extras$mcl)

  idxs <- match(mcl_extras$pcode, d$pcode)
  d[idxs, colnames(mcl_extras)] <- mcl_extras[]

  d$mcl[idxs] <- mcl_extras$mcl |> as.numeric()
  d$parm_unit[idxs] <- mcl_extras$parm_unit

  is <- apply(is.na(d[, bm]), 1, all)
  d <- d[!is, ]

  d$parm_unit <- as.factor(d$parm_unit)

  idxs <- tolower(d$srsname) |> stringi::stri_order(numeric = TRUE)
  cols <- c(
    "srsname",
    "pcode",
    "parm_unit",
    "mcl",
    "hhbp_noncancer",
    "hhbp_cancer_min",
    "hhbp_cancer_max",
    "hbsl_noncancer",
    "hbsl_cancer_min",
    "hbsl_cancer_max",
    "remark"
  )
  d <- d[idxs, cols]
  rownames(d) <- NULL

  d
}


# Site Information (sites) -----

mds_sites <- function(file, crs) {

  # check arguments
  checkmate::assert_string(file)
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_class(crs, "crs")

  # read metadata file
  meta <- utils::read.delim(file = file, header = FALSE, colClasses = "character")
  colnames(meta) <- c("agency_cd", "site_no", "station_nm", "network_cd", "pos")

  # check for duplicated values
  if (any(is <- duplicated(meta$site_no))) {
    txt <- sQuote(meta$site_no[is]) |> paste(collapse = ", ")
    stop("Duplicated site numbers: ", txt, call. = FALSE)
  }

  # download data from NWIS
  d <- dataRetrieval::readNWISsite(siteNumbers = unique(meta$site_no))

  # change class
  meta$pos <- as.integer(meta$pos)

  # merge in metadata
  d <- merge(d, meta[, c("site_no", "network_cd", "pos")], by = "site_no")

  # check altitude datum
  is <- !(d$alt_datum_cd %in% "NAVD88")
  if (any(is)) {
    warning(
      "Unrecognized altitude datum code in sites, will assume NAVD88:",
      call. = FALSE,
      immediate. = TRUE
    )
    sprintf("  %s (%s) datum '%s'", d$site_no[is], d$station_nm[is], d$alt_datum_cd[is]) |>
      paste(collapse = "\n") |> message()
    d$alt_datum_cd[is] <- "NAVD88"
  }

  d$site_nm <- parse_station_nm(d$station_nm)
  d$station_nm <- trim_station_nm(d$station_nm)

  d$completion_cd <- "O"
  is <- grepl(" PORT", d$station_nm) & grepl(" ZONE", d$station_nm)
  d$completion_cd[is] <- "M"
  x <- d[, c("lat_va", "long_va")]
  is <- !is & (duplicated(x) | duplicated(x, fromLast = TRUE))
  d$completion_cd[is] <- "P"
  is <- d$network_cd == "S"
  d$completion_cd[is] <- NA_character_

  d$construction_dt <- as.character(d$construction_dt) |>
    as.Date(tryFormats = c("%Y%m%d", "%Y%m", "%Y"))

  cols <- c(
    "coord_meth_cd",
    "coord_acy_cd",
    "alt_meth_cd",
    "reliability_cd",
    "aqfr_type_cd",
    "nat_aqfr_cd",
    "aqfr_cd",
    "depth_src_cd",
    "completion_cd",
    "huc_cd",
    "network_cd"
  )
  for (i in cols) {
    d[[i]][d[[i]] == ""] <- NA_character_
    d[[i]] <- as.factor(d[[i]])
  }

  idxs <- order(
    d$network_cd,
    stringi::stri_rank(tolower(d$site_nm), numeric = TRUE),
    d$well_depth_va
  )
  cols <- c(
    "dec_long_va",
    "dec_lat_va",
    "site_nm",
    "station_nm",
    "site_no",
    "coord_meth_cd",
    "coord_acy_cd",
    "alt_va",
    "alt_meth_cd",
    "alt_acy_va",
    "huc_cd",
    "construction_dt",
    "reliability_cd",
    "nat_aqfr_cd",
    "aqfr_cd",
    "aqfr_type_cd",
    "well_depth_va",
    "hole_depth_va",
    "depth_src_cd",
    "completion_cd",
    "network_cd",
    "pos"
  )
  d <- d[idxs, cols]
  rownames(d) <- NULL
  d <- droplevels(d)

  sp <- sf::st_as_sf(d,
    coords = c("dec_long_va", "dec_lat_va"),
    crs = sf::st_crs("+proj=longlat +datum=NAD83")
  ) |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs)

  sp
}


# Surface-Water Measurements (swm) -----

mds_swm <- function(tz, sites) {

  # check arguments
  checkmate::assert_string(tz)
  checkmate::assert_class(sites, classes = "sf")

  # download data from NWIS
  d <- dataRetrieval::readNWISmeas(
    siteNumbers = sites$site_no,
    convertType = TRUE,
    tz = tz
  )

  # add local site names
  idxs <- match(d$site_no, sites$site_no)
  d$site_nm <- sites$site_nm[idxs]

  # set measurement timestamp
  if (anyNA(d$measurement_dateTime)) {
    stop("Missing timestamp", call. = FALSE)
  }
  d$stage_dt <- d$measurement_dateTime

  # remove duplicated timestamp records
  is <- duplicated(d[, c("site_no", "stage_dt")])
  if (any(is)) {
    warning("Removed duplicated timestamp records:", call. = FALSE, immediate. = TRUE)
    sprintf("  %s (%s) at %s", d$site_no[is], d$site_nm[is], d$stage_dt[is]) |>
      paste(collapse = "\n") |> message()
    d <- d[!is, ]
  }

  # set measurement values
  d$stage_va <- as.numeric(d$gage_height_va)
  d$disch_va <- as.numeric(d$discharge_va)
  is <- is.na(d$stage_va) & is.na(d$disch_va)
  d <- d[!is, ]

  # add measurment accuracy
  qual <- as.character(d$measured_rating_diff)
  per_unc <- c(
    "Excellent" = 2,
    "Good" = 5,
    "Fair" = 8,
    "Poor" = 10,
    "Unknown" = NA_real_,
    "Unspecified" = NA_real_
  )
  idxs <- match(qual, names(per_unc))
  d$frac_unc <- per_unc[idxs] / 100
  d$stage_acy_va <- (d$stage_va * d$frac_unc) |> round_usgs(digits = 2)
  d$disch_acy_va <- (d$disch_va * d$frac_unc) |> round_usgs(digits = 2)

  # sort records
  idxs <- order(
    stringi::stri_rank(tolower(d$site_nm), numeric = TRUE),
    d$site_no,
    d$stage_dt
  )
  cols <- c(
    "site_nm",
    "site_no",
    "stage_dt",
    "stage_va",
    "disch_va",
    "stage_acy_va",
    "disch_acy_va"
  )
  d <- d[idxs, cols]
  rownames(d) <- NULL

  d
}


# Groundwater Levels (gwl) -----

mds_gwl <- function(tz, sites) {

  # check arguments
  checkmate::assert_string(tz)
  checkmate::assert_class(sites, classes = "sf")

  # download data from NWIS
  d <- dataRetrieval::readNWISgwl(
    siteNumbers = sites$site_no,
    parameterCd = c(
      "72019", # depth to water level, in feet below land surface.
      "62611" # groundwater level above NAVD 88, in feet
    ),
    convertType = TRUE,
    tz = tz
  )

  # add local site names
  idxs <- match(d$site_no, sites$site_no)
  d$site_nm <- sites$site_nm[idxs]

  # set measurement data-time
  ct <- d$lev_dateTime
  is <- is.na(ct)
  ct[is] <- paste(d$lev_dt[is], "12:00") |>
    as.POSIXct(tz = tz, format = "%Y-%m-%d %H:%M")
  d$lev_dt <- ct
  is <- is.na(d$lev_dt)
  if (any(is)) {
    warning("Unknown time formats, removed records:", call. = FALSE, immediate. = TRUE)
    sprintf("  %s (%s) format '%s'", d$site_no[is], d$site_nm[is], d$lev_dateTime[is]) |>
      paste(collapse = "\n") |> message()
  }
  d <- d[!is, ]

  # set measurement values
  d$lev_va <- as.numeric(d$lev_va)
  d$sl_lev_va <- as.numeric(d$sl_lev_va)

  # place water-level depth and elevation on same row
  d$id <- paste(d$site_no, d$site_tp_cd, d$lev_dt)
  sl <- d[d$parameter_cd == "62611", c("id", "sl_lev_va")]
  cols <- c(
    "id",
    "site_nm",
    "site_no",
    "lev_dt",
    "lev_acy_cd",
    "lev_meth_cd",
    "lev_status_cd",
    "lev_age_cd",
    "lev_va"
  )
  d <- d[d$parameter_cd == "72019", cols]
  d <- merge(d, sl, by = "id")
  d$id <- NULL

  # remove missing values
  is <- is.na(d$lev_va) | is.na(d$sl_lev_va)
  d <- d[!is, ]

  # average daily values when missing time and all other columns are equal
  is <- colnames(d) %in% c("lev_va", "sl_lev_va")
  ids <- apply(d[, !is], MARGIN = 1, FUN = paste, collapse = " ")
  agg <- stats::aggregate(d[, is], by = list("ids" = ids), FUN = mean)
  idxs <- match(ids, agg$ids)
  d$lev_va <- agg$lev_va[idxs]
  d$sl_lev_va <- agg$sl_lev_va[idxs]
  d <- d[!duplicated(ids), ]

  # add groundwater level accuracy values
  d$lev_acy_va <- NA_real_
  lev_acy <- c("0" = 1, "1" = 0.1, "2" = 0.01)
  d$lev_acy_va <- lev_acy[match(d$lev_acy_cd, names(lev_acy))]
  d$lev_acy_cd <- NULL
  d$sl_lev_acy_va <- sites$alt_acy_va[match(d$site_no, sites$site_no)] + d$lev_acy_va

  d$lev_meth_cd <- as.factor(d$lev_meth_cd)
  d$lev_status_cd <- as.factor(d$lev_status_cd)
  d$lev_age_cd <- as.factor(d$lev_age_cd)

  # sort records
  idxs <- order(
    stringi::stri_rank(tolower(d$site_nm), numeric = TRUE),
    d$site_no,
    d$lev_dt
  )
  d <- d[idxs, ]
  rownames(d) <- NULL

  d
}


# Parameter Units (units) -----

mds_units <- function(file) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")

  # read file
  d <- utils::read.delim(file, comment.char = "#", na.strings = "", colClasses = "character")

  idxs <- tolower(d$parm_unit) |> order()
  d <- d[idxs, ]
  rownames(d) <- NULL

  d
}


# Background Concentrations (background) -----

mds_background <- function(file) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")

  d <- utils::read.delim(file, strip.white = TRUE, colClasses = "character")

  d$bkgrd_min <- as.numeric(d$bkgrd_min)
  d$bkgrd_max <- as.numeric(d$bkgrd_max)
  d$reference <- as.factor(d$reference)
  d$parm_unit <- as.factor(d$parm_unit)

  idxs <- tolower(d$srsname) |> stringi::stri_order(numeric = TRUE)
  d <- d[idxs, ]
  rownames(d) <- NULL

  d
}


# Eastern Snake River Plain Boundary (esrp) -----

mds_esrp <- function(file, crs) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_class(crs, classes = "crs")

  sp <- sf::st_read(dsn = file) |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs, check = TRUE)
  geometry <- sf::st_sfc(sp$geometry)
  sp <- sf::st_sf(geometry)

  sp
}


# Idaho National Laboratory Boundary (inl) -----

mds_inl <- function(file, crs) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_class(crs, classes = "crs")

  sp <- sf::st_read(dsn = file) |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs, check = TRUE)
  geometry <- sf::st_sfc(sp$geometry)
  sp <- sf::st_sf(geometry)

  sp
}


# Industrial Waste Ditch (iwd) -----

mds_iwd <- function(file, crs) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_class(crs, classes = "crs")

  sp <- sf::st_read(dsn = file) |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs)
  geometry <- sf::st_sfc(sp$geometry)
  sp <- sf::st_sf(geometry)

  sp
}


# Idaho National Laboratory Facilities (facilities) -----

mds_facilities <- function(file, crs) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_class(crs, classes = "crs")

  sp <- sf::st_read(dsn = file, agr = "identity") |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs, check = TRUE)
  names(sp) <- c("id", "geometry")

  features <- c(
    "ATRC" = "Advanced Test Reactor Complex",
    "CFA" = "Central Facilities Area",
    "INTEC" = "Idaho Nuclear Technology and Engineering Center",
    "MFC" = "Materials and Fuels Complex",
    "NRF" = "Naval Reactors Facility",
    "RWMC" = "Radioactive Waste Management Complex",
    "TAN" = "Test Area North "
  )
  sp$name <- features[match(sp$id, names(features))]

  idxs <- tolower(sp$name) |> stringi::stri_order(numeric = TRUE)
  cols <- c("name", "id", "geometry")
  sp <- sp[idxs, cols]
  rownames(sp) <- NULL

  sp
}


# Percolation Ponds (percponds) -----

mds_percponds <- function(file, crs) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_class(crs, classes = "crs")

  sp <- sf::st_read(dsn = file, agr = "identity") |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs, check = TRUE)

  cols <- c("name" = "Name", "facility_id" = "Pond_loc")
  sp <- sp[, cols]
  names(sp) <- c(names(cols), "geometry")

  sp$facility_id <- as.factor(sp$facility_id)

  idxs <- order(
    stringi::stri_rank(tolower(sp$name), numeric = TRUE),
    stringi::stri_rank(tolower(sp$facility_id), numeric = TRUE)
  )
  sp <- sp[idxs, ]
  rownames(sp) <- NULL

  sp
}


# Mountain Ranges and Buttes (mountains) -----

mds_mountains <- function(file, crs, sp_extent) {

  # check arguments
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(sp_extent, "bbox")

  sp <- sf::st_read(dsn = file, agr = "identity") |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs, check = TRUE) |>
    sf::st_crop(sp_extent)

  idxs <- tolower(sp$name) |> stringi::stri_order(numeric = TRUE)
  sp <- sp[idxs, ]
  rownames(sp) <- NULL

  sp
}


# State of Idaho Boundary (idaho) -----

mds_idaho <- function(url, cachedir, crs) {

  # check arguments
  checkmate::assert_string(url)
  checkmate::assert_directory_exists(cachedir, access = "rw")
  checkmate::assert_class(crs, classes = "crs")

  file <- file.path(cachedir, basename(url))
  if (!checkmate::test_file_exists(file)) {
    assert_url(url)
    utils::download.file(url = url, destfile = file, mode = "wb")
  }
  files <- utils::unzip(file, exdir = tempdir())
  sp <- grep(".shp$", files, value = TRUE) |>
    sf::st_read() |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs, check = TRUE) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 100)

  is <- sp[["NAME"]] == "Idaho"
  geometry <- sf::st_sfc(sp[is, ]$geometry)
  sp <- sf::st_sf(geometry)

  sp
}


# Cities and Towns (cities) -----

mds_cities <- function(url, cachedir, crs, sp_extent) {

  # check arguments
  checkmate::assert_string(url)
  checkmate::assert_directory_exists(cachedir, access = "rw")
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(sp_extent, "bbox")

  file <- file.path(cachedir, basename(url))
  if (!checkmate::test_file_exists(file)) {
    assert_url(url)
    utils::download.file(url = url, destfile = file, mode = "wb")
  }
  files <- utils::unzip(file, exdir = tempdir())
  sp <- grep(".shp$", files, value = TRUE) |>
    sf::st_read(agr = "constant") |>
    sf::st_make_valid() |>
    sf::st_centroid() |>
    sf::st_transform(crs = crs) |>
    sf::st_crop(sp_extent)

  cols <- c("name" = "NAME", "id" = "GEOID")
  sp <- sp[, cols]
  names(sp) <- c(names(cols), "geometry")

  sf::st_agr(sp) <- c("name" = "identity", "id" = "identity")

  idxs <- tolower(sp$name) |> stringi::stri_order(numeric = TRUE)
  sp <- sp[idxs, ]
  rownames(sp) <- NULL

  sp
}


# County Boundaries (counties) -----

mds_counties <- function(url, cachedir, crs, sp_extent) {

  # check arguments
  checkmate::assert_string(url)
  checkmate::assert_directory_exists(cachedir, access = "rw")
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(sp_extent, "bbox")

  file <- file.path(cachedir, basename(url))
  if (!checkmate::test_file_exists(file)) {
    assert_url(url)
    utils::download.file(url = url, destfile = file, mode = "wb")
  }
  files <- utils::unzip(file, exdir = tempdir())
  sp <- grep(".shp$", files, value = TRUE) |>
    sf::st_read(agr = "constant") |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs, check = TRUE) |>
    sf::st_crop(sp_extent)

  cols <- c("name" = "NAME", "id" = "GEOID")
  sp <- sp[, cols]
  names(sp) <- c(names(cols), "geometry")

  sf::st_agr(sp) <- c("name" = "identity", "id" = "identity")

  idxs <- tolower(sp$name) |> stringi::stri_order(numeric = TRUE)
  sp <- sp[idxs, ]
  rownames(sp) <- NULL

  sp
}


# Road Netowrk (roads) -----

mds_roads <- function(urls, cachedir, crs, sp_extent) {

  # check arguments
  checkmate::assert_character(urls, len = 2, any.missing = FALSE)
  checkmate::assert_directory_exists(cachedir, access = "rw")
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(sp_extent, "bbox")

  # read primary and secondary roads file
  file <- file.path(cachedir, basename(urls[1]))
  if (!checkmate::test_file_exists(file)) {
    assert_url(urls[1])
    utils::download.file(url = urls[1], destfile = file, mode = "wb")
  }
  files <- utils::unzip(file, exdir = tempdir())
  prisec_roads <- grep(".shp$", files, value = TRUE) |>
    sf::st_read() |>
    sf::st_make_valid()

  # read all roads file
  file <- file.path(cachedir, basename(urls[2]))
  if (!checkmate::test_file_exists(file)) {
    assert_url(urls[2])
    utils::download.file(url = urls[2], destfile = file, mode = "wb")
  }
  files <- utils::unzip(file, exdir = tempdir())
  all_roads <- grep(".shp$", files, value = TRUE) |>
    sf::st_read() |>
    sf::st_make_valid()

  is <- !(all_roads[["LINEARID"]] %in% prisec_roads[["LINEARID"]])
  other_roads <- all_roads[is, ]

  prisec_roads$prisec_fl <- TRUE
  other_roads$prisec_fl <- FALSE

  sp <- rbind(prisec_roads, other_roads) |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs) |>
    sf::st_crop(sp_extent)
  rownames(sp) <- NULL

  cols <- c(
    "name" = "FULLNAME",
    "id" = "LINEARID",
    "route_tp" = "RTTYP",
    "prisec_fl" = "prisec_fl"
  )
  sp <- sp[, cols]
  colnames(sp) <- c(names(cols), "geometry")

  sp$route_tp <- as.factor(sp$route_tp)

  idxs <- order(
    stringi::stri_rank(tolower(sp$name), numeric = TRUE),
    sp$id
  )
  sp <- sp[idxs, ]
  rownames(sp) <- NULL

  sp
}


# Lakes and Ponds (lakes) -----

mds_lakes <- function(url, cachedir, crs, sp_extent) {

  # check arguments
  checkmate::assert_string(url)
  checkmate::assert_directory_exists(cachedir, access = "rw")
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(sp_extent, "bbox")

  file <- file.path(cachedir, basename(url))
  if (!checkmate::test_file_exists(file)) {
    assert_url(url)
    utils::download.file(url = url, destfile = file, mode = "wb")
  }
  sprintf("7z e -aoa -bd -o\"%s\" \"%s\"", tempdir(), file) |> system()
  sp <- file.path(tempdir(), "NHDWaterbody.shp") |>
    sf::st_read(agr = "constant") |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs, check = TRUE) |>
    sf::st_crop(sp_extent)

  cols <- c(
    "gnis_nm" = "GNIS_NAME",
    "id" = "COMID",
    "reach_cd" = "REACHCODE",
    "gnis_id" = "GNIS_ID",
    "feature_tp" = "FTYPE"
  )
  sp <- sp[, cols]
  names(sp) <- c(names(cols), "geometry")

  sf::st_agr(sp) <- c(
    "gnis_nm" = "identity",
    "id" = "identity",
    "reach_cd" = "identity",
    "gnis_id" = "identity",
    "feature_tp" = "constant"
  )

  sp$id <- as.character(sp$id)
  sp$feature_tp <- as.factor(sp$feature_tp)

  idxs <- order(
    stringi::stri_rank(tolower(sp$gnis_nm), numeric = TRUE),
    sp$id
  )
  sp <- sp[idxs, ]
  rownames(sp) <- NULL

  sp
}


# Rivers and Streams (streams) -----

mds_streams <- function(url, cachedir, crs, sp_extent) {

  # check arguments
  checkmate::assert_string(url)
  checkmate::assert_directory_exists(cachedir, access = "rw")
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(sp_extent, "bbox")

  file <- file.path(cachedir, basename(url))
  if (!checkmate::test_file_exists(file)) {
    assert_url(url)
    utils::download.file(url = url, destfile = file, mode = "wb")
  }
  sprintf("7z e -aoa -bd -o\"%s\" \"%s\"", tempdir(), file) |> system()
  sp <- file.path(tempdir(), "NHDFlowline.shp") |>
    sf::st_read(agr = "constant") |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs) |>
    sf::st_crop(sp_extent)

  cols <- c(
    "gnis_nm" = "GNIS_NAME",
    "id" = "COMID",
    "reach_cd" = "REACHCODE",
    "gnis_id" = "GNIS_ID",
    "feature_tp" = "FTYPE",
    "resolution_cd" = "RESOLUTION"
  )
  sp <- sp[, cols]
  names(sp) <- c(names(cols), "geometry")

  sf::st_agr(sp) <- c(
    "gnis_nm" = "identity",
    "id" = "identity",
    "reach_cd" = "identity",
    "gnis_id" = "identity",
    "feature_tp" = "constant",
    "resolution_cd" = "constant"
  )

  is <- !is.na(sp$gnis_id)
  sp <- sp[is, ]

  sp$id <- as.character(sp$id)
  sp$feature_tp <- as.factor(sp$feature_tp)
  sp$resolution_cd <- as.factor(sp$resolution_cd)

  idxs <- order(
    stringi::stri_rank(tolower(sp$gnis_nm), numeric = TRUE),
    sp$id
  )
  sp <- sp[idxs, ]
  rownames(sp) <- NULL

  sp
}


# Digital Elevation Model (dem) -----

mds_dem <- function(urls, cachedir, crs, sp_extent) {

  # check arguments
  checkmate::assert_character(urls, min.len = 1, any.missing = FALSE)
  checkmate::assert_directory_exists(cachedir, access = "rw")
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(sp_extent, "bbox")

  files <- file.path(cachedir, basename(urls))

  is <- !vapply(
    files,
    FUN = checkmate::test_file_exists,
    FUN.VALUE = logical(1),
    access = "r"
  )
  if (any(is)) {
    for (url in urls[is]) assert_url(url)
    utils::download.file(
      url = urls[is],
      destfile = files[is],
      method = "libcurl",
      mode = "wb"
    )
  }

  grd <- terra::rast(
    crs = crs$input,
    extent = terra::ext(sp_extent),
    resolution = 100
  )
  grd_disagg <- terra::disagg(grd, fact = 5L)

  args <- lapply(files, FUN = terra::rast)
  dem <- do.call(terra::merge, args)

  sr <- dem |>
    terra::project(grd_disagg) |>
    terra::aggregate(fact = 5L, fun = stats::median)
  sr[] <- sr[] * 3.2808399 # convert meters to feet
  sr[] <- round_usgs(sr[], digits = 0)
  names(sr) <- "elevation"

  terra::wrap(sr)
}

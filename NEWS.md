# inldata 1.1.5

- Reduce ellapsed time for `make_data_release` function example.

# inldata 1.1.4

- Export 'internal' functions to avoid using `:::` in examples.
- Replace `\dontrun{}` with `\donttest{}` in the `assert_url` function example.
- Avoid using `print()` within functions.

# inldata 1.1.3

- Address issues with failing CRAN checks: remove hyperlink and refactor CITATION file.

# inldata 1.1.2

- Decrease run time (< 5s) for example in `make_data_release` function help documentation.

# inldata 1.1.1

- Fix popups in leaflet map markers.

# inldata 1.1.0

- Add latest data records.
- Add data for new monitoring sites.
- Add vignettes to package website.
- Display summary tables for sites and parameters in package vignettes.
- Display Entity Relationship Diagram (ERD) in package vignette.
- Replace `projection` dataset (text string) with `crs` dataset (crs-class object).
- Rename `topo` dataset to `dem`, convert elevation from meters to feet, and remove hill shade layer.
- Add `make_shade` function, used to calculate hill shade from a elevation raster.
- Add `make_data_release` function, used to create a data release.
- Add `make_datasets` function, used to create package datasets.
- Add and rename fields in many of the datasets.

# inldata 1.0.4

- Rename Git branch from `master` to `main`.
- Remove `labels` dataset.
- Build website using **pkgdown**.
- Remove **sp** and **raster** package dependencies, replaced with **sf** and **terra** packages.
- Fix broken URL's.
- Update license.
- Re-style code based on static code analysis using the **lintr** package.

# inldata 1.0.3

- Fix invalid URL.

# inldata 1.0.2

- Fix invalid URL's.

# inldata 1.0.1

- In DESCRIPTION file, move **sp** and **raster** form Suggests to Imports.

# inldata 1.0.0

- Host repo on USGS OpenSource GitLab (code.usgs.gov)

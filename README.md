inldata
=======

[![USGS
Category](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)
[![pipeline
status](https://code.usgs.gov/inl/inldata/badges/master/pipeline.svg)](https://code.usgs.gov/inl/inldata/-/commits/master)
[![CRAN
Version](https://www.r-pkg.org/badges/version/inldata)](https://CRAN.R-project.org/package=inldata)

Description
-----------

The [R](https://www.r-project.org/) package **inldata** is a collection
of datasets for the U.S. Geological Survey-Idaho National Laboratory
aquifer monitoring networks administrated by the [Idaho National
Laboratory Project
Office](https://www.usgs.gov/centers/id-water/science/idaho-national-laboratory-project-office)
in cooperation with the U.S. Department of Energy. Data collected from
wells at the Idaho National Laboratory have been used to describe the
effects of waste disposal on water contained in the eastern Snake River
Plain aquifer, located in the southeastern part of Idaho, and the
availability of water for long-term consumptive and industrial use.
Included in this package are the long-term monitoring records, dating
back to measurements from 1949, and the geospatial data describing the
areas from which samples were collected or observations were made.
Bundling this data into a single R package significantly reduces the
magnitude of data processing for researches. And provides a way to
distribute the data along with its documentation in a standard format.
Geospatial datasets are made available in a common projection and datum,
and geohydrologic data have been structured to facilitate analysis. A
list of all datasets in the package is given below.

| Dataset      | Title                                |
|:-------------|:-------------------------------------|
| `background` | Background Concentrations            |
| `benchmarks` | Benchmark Concentrations             |
| `cities`     | Cities and Towns                     |
| `counties`   | County Boundaries                    |
| `dl`         | Laboratory Detection Limits          |
| `esrp`       | Eastern Snake River Plain Boundary   |
| `facilities` | Idaho National Laboratory Facilities |
| `gwl`        | Groundwater Level Measurements       |
| `idaho`      | State of Idaho Boundary              |
| `inl`        | Idaho National Laboratory Boundary   |
| `labels`     | Map Labels                           |
| `lakes`      | Lakes and Ponds                      |
| `mountains`  | Mountain Ranges and Buttes           |
| `parameters` | Parameter Information for Analytes   |
| `projection` | Coordinate Reference System          |
| `roads`      | Primary and Secondary Roads          |
| `samples`    | Water-Quality Data Records           |
| `sites`      | Descriptive Site Information         |
| `streams`    | Rivers and Streams                   |
| `topo`       | Land-Surface Topography              |

Installation
------------

The current release is available on
[CRAN](https://CRAN.R-project.org/package=inldata "The Comprehensive R Archive Network"),
which you can install using the following command:

``` r
install.packages("inldata")
```

To also install user-contributed packages that **inldata** depends on to
(1) run examples in the package help documentation, and (2) run code
used to build package datasets, run:

``` r
install.packages("inldata", dependencies = TRUE)
```

To install the development version, you need to clone the repository and
build from source, or run:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_gitlab("inl/inldata", host = "code.usgs.gov",
                        dependencies = TRUE)
```

Usage
-----

Examples are given in the package help pages. To access this
documentation, run:

``` r
library("inldata")
help(package = "inldata")
```

Author
------

Jason C. Fisher (ORCID iD
[0000-0001-9032-8912](http://orcid.org/0000-0001-9032-8912))

Point of Contact
----------------

Jason C. Fisher
(<a href="mailto:jfisher@usgs.gov" class="email">jfisher@usgs.gov</a>)

Suggested Citation
------------------

To cite **inldata** in publications, please use:

Fisher, J.C., 2020, inldata—Collection of datasets for the U.S.
Geological Survey-Idaho National Laboratory Aquifer Monitoring Networks:
U.S. Geological Survey software release, R package, Reston, Va.,
<a href="https://doi.org/10.5066/P9PP9UXZ" class="uri">https://doi.org/10.5066/P9PP9UXZ</a>.

Contributing
------------

We welcome your contributions and suggestions for how to make these
materials more useful to the community. Please feel free to comment on
the [issue tracker](https://code.usgs.gov/inl/inldata/-/issues) or open
a [merge request](https://code.usgs.gov/inl/inldata/-/merge_requests) to
contribute.

Code of Conduct
---------------

All contributions to- and interactions surrounding- this project will
abide by the [USGS Code of Scientific
Conduct](https://www.usgs.gov/about/organization/science-support/science-quality-and-integrity/fundamental-science-practices).

Disclaimer
----------

This software has been approved for release by the U.S. Geological
Survey (USGS). Although the software has been subjected to rigorous
review, the USGS reserves the right to update the software as needed
pursuant to further analysis and review. No warranty, expressed or
implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of
release constitute any such warranty. Furthermore, the software is
released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.

Although these data have been processed successfully on a computer
system at the USGS, no warranty expressed or implied is made regarding
the display or utility of the data for other purposes, nor on all
computer systems, nor shall the act of distribution constitute any such
warranty. The USGS or the U.S. Government shall not be held liable for
improper or incorrect use of the data described and/or contained herein.

Any use of trade, product, or firm names is for descriptive purposes
only and does not imply endorsement by the U.S. Government.

License Information
-------------------

This software is a product of the U.S. Geological Survey (USGS), an
agency of the United States Department of Interior, which is part of the
U.S. Government.

#### Cost

This software is freely distributed. There is no fee to download and
(or) use this software.

#### License

Users do not need a license or permission from the USGS to use this
software. Users can download and install as many copies of the software
as they need. See Exceptions below.

#### Public domain

As a work of the United States Government, this USGS product is in the
[public domain](https://www.usa.gov/government-works) within the United
States. You can copy, modify, distribute, and perform the work, even for
commercial purposes, all without asking permission. Additionally, USGS
waives copyright and related rights in the work worldwide through [CC0
1.0 Universal Public Domain
Dedication](https://creativecommons.org/publicdomain/zero/1.0/).

#### Exceptions

This project may have numerous dependencies from other open-source
software projects. The re-use and distribution of those software
packages may be subject to the licenses of each of those dependencies.

Support
-------

The Idaho National Laboratory Project Office of the USGS supports the
development and maintenance of **inldata**. Resources are available
primarily for maintenance and responding to user questions. Priorities
on the development of new features are determined by the development
team.

Disclaimer
----------

This software has been approved for release by the U.S. Geological
Survey (USGS). Although the software has been subjected to rigorous
review, the USGS reserves the right to update the software as needed
pursuant to further analysis and review. No warranty, expressed or
implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of
release constitute any such warranty. Furthermore, the software is
released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.

Although these data have been processed successfully on a computer
system at the USGS, no warranty expressed or implied is made regarding
the display or utility of the data for other purposes, nor on all
computer systems, nor shall the act of distribution constitute any such
warranty. The USGS or the U.S. Government shall not be held liable for
improper or incorrect use of the data described and/or contained herein.

Any use of trade, product, or firm names is for descriptive purposes
only and does not imply endorsement by the U.S. Government.

Additional Publication Details
------------------------------

Additional metadata about this publication, not found in other parts of
the page is in this table.

<!--html_preserve-->
<table>
<tbody>
<tr>
<th scope="row">
Publication type
</th>
<td>
Formal R language package
</td>
</tr>
<tr>
<th scope="row">
DOI
</th>
<td>
10.5066/P9PP9UXZ
</td>
</tr>
<tr>
<th scope="row">
Year published
</th>
<td>
2020
</td>
</tr>
<tr>
<th scope="row">
Year of version
</th>
<td>
2020
</td>
</tr>
<tr>
<th scope="row">
Version
</th>
<td>
1.0.3
</td>
</tr>
<tr>
<th scope="row">
IPDS
</th>
<td>
IP-120865
</td>
</tr>
</tbody>
</table>

<cr><!--/html_preserve-->

if (Sys.getenv("CI") == "true") {
  install.packages("hrbrthemes", repos = "https://cinc.rud.is")
  package = "https://cran.r-project.org/package=rjson&version=0.2.19"
  utils::install.packages(pkgs = package, repos = NULL)
}

## Auto-Install packages
.packs <- c("gdata", "zoo", "testthat", "readr", "maptools",
            "ggplot2", "rgdal", "lubridate", "stringr",
            "foreign", "jsonlite", "xtable", "dplyr", "tidyr",
            "rgeos", "maps", "sp", "tidyr", "pointdensityP",
            "ggmap", "mgcv", "viridis", "spdep", "ggrepel",
            "tufte", "parallel", "mapproj")
.success <- suppressPackageStartupMessages(
  suppressWarnings(sapply(.packs, require, character.only = TRUE))
)
if (length(names(.success)[!.success])) {
  install.packages(names(.success)[!.success])
  sapply(names(.success)[!.success], require, character.only = TRUE)
}

options(stringsAsFactors = FALSE)
theme_set(theme_bw())

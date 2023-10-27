if (Sys.getenv("CI") == "true") {
  inst_package <- function(packageurl) {
    install.packages(packageurl, repos=NULL, type="source")
  }
  
  install.packages("hrbrthemes", repos = "https://cinc.rud.is")
  package = "https://cran.r-project.org/package=rjson&version=0.2.19"
  utils::install.packages(pkgs = package, repos = NULL)
  
  tryCatch({
    packageurl <- 'https://cran.r-project.org/src/contrib/RgoogleMaps_1.4.5.3.tar.gz'
    if (packageVersion("RgoogleMaps") != "1.4.5.3") {
      inst_package(packageurl)}
  }, 
  error = function(x) {inst_package(packageurl)}
  )
  tryCatch({
    packageurl <- 'https://cran.r-project.org/src/contrib/Archive/cli/cli_2.0.2.tar.gz'
    if (packageVersion("RgoogleMaps") != "2.0.2") {
      inst_package(packageurl)}
  }, 
  error = function(x) {inst_package(packageurl)}
  )
  tryCatch({
    packageurl <- 'https://cran.r-project.org/src/contrib/Archive/ggmap/ggmap_3.0.0.tar.gz'
    if (packageVersion("RgoogleMaps") != "3.0.0") {
      inst_package(packageurl)}
  }, 
  error = function(x) {inst_package(packageurl)}
  )
  
  tryCatch({
    packageurl <- 'https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.5-28.tar.gz'
    if (packageVersion("rgdal") != "1.5.28") {
      inst_package(packageurl)}
  }, 
    error = function(x) {inst_package(packageurl)}
  )
  
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

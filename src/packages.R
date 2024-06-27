if (Sys.getenv("CI") == "true") {
  inst_package <- function(packageurl) {
    install.packages(packageurl, repos=NULL, type="source")
  }

  packageurl <- 'https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.5-28.tar.gz'
  inst_package(packageurl)

  # packageurl <- 'https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.5-8.tar.gz	'
  # inst_package(packageurl)
  # 
  # packageurl <- 'https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-7.tar.gz'
  # inst_package(packageurl)
}

#maptools, rgeos, rgdal, rstanarm
## Auto-Install packages
# .packs <- c("gdata", "zoo", "testthat", "readr", 
#             "ggplot2",  "lubridate", "stringr",
#             "foreign", "jsonlite", "xtable", "dplyr", "tidyr",
#             "maps", "sp", "tidyr", "pointdensityP",
#             "ggmap", "mgcv", "viridis", "spdep", "ggrepel",
#             "tufte", "parallel", "mapproj", "cli", "geojsonio")
# .success <- suppressPackageStartupMessages(
#   suppressWarnings(sapply(.packs, require, character.only = TRUE))
# )
# if (length(names(.success)[!.success])) {
#   install.packages(names(.success)[!.success])
#   sapply(names(.success)[!.success], require, character.only = TRUE)
# }


library(gdata)
library(zoo)
library(testthat)
library(readr)

library(ggplot2)
library(lubridate)
library(stringr)

library(foreign)
library(jsonlite)
library(xtable)
library(dplyr)
library(tidyr)

library(maps)
library(sp)
library(tidyr)
library(pointdensityP)

library(ggmap)
library(mgcv)
library(viridis)
library(spdep)
library(ggrepel)

library(tufte)
library(parallel)
library(mapproj)
library(cli)
library(geojsonio)
library(hrbrthemes)
library(rstanarm)


library(rgdal)
# library(rgeos)


options(stringsAsFactors = FALSE)
theme_set(theme_bw())

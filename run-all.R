# Program to download and clean the data from 
#
# Carpetas de investigación PGJ de la Ciudad de México
# https://datos.cdmx.gob.mx/explore/dataset/carpetas-de-investigacion-pgj-cdmx/information/
#
# to be used in hoyodecrimen.com
# Sys.getenv("RENV_PATHS_CACHE")
# Sys.setenv(RENV_PATHS_ROOT = "/renv/.cache/R/Env")
renv::restore(rebuild = FALSE, prompt = FALSE)
cuadrantes_date <- 2023 # Use the 2023 or 2016 cuadrantes shapefile
use_cores <- parallel::detectCores()


main <- function() {
  source(file.path("src", "packages.R"))
  # Clean PGJ-CDMX Carpetas data
  source(file.path("src", "clean-pgj-carpetas.R"))
  # Clean PGJ-CDMX Víctimas data
  source(file.path("src", "clean-pgj-victimas.R"))
  # Clean SESNSP data
  source(file.path("src", "sesnsp.R"))
  # Analysis
  source(file.path("src", "gam_colonias_carpetas.R"))
  source(file.path("src", "homicide_geojson.R"))
  source(file.path("src", "multilevel_gam.R"))
}
main()


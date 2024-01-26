# Program to download and clean the data from 
#
# Carpetas de investigación PGJ de la Ciudad de México
# https://datos.cdmx.gob.mx/explore/dataset/carpetas-de-investigacion-pgj-cdmx/information/
#
# to be used in hoyodecrimen.com



main <- function() {
  source(file.path("src", "packages.R"))
  # Clean PGJ-CDMX data
  source(file.path("src", "clean-pgj.R"))
  # Clean SESNSP data
  source(file.path("src", "sesnsp.R"))
  # Analysis
  source(file.path("src", "gam.R"))
  register_stadiamaps(Sys.getenv("STADIA_KEY"), write = FALSE)
  source(file.path("src", "pointdensity.R"))
  source(file.path("src", "pointdensity_dates.R"))
  source(file.path("src", "multilevel_gam.R"))
}
main()


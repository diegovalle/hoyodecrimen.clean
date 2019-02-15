# Clean PGJ-CDMX data
source(file.path("src", "packages.R"))
source(file.path("src", "clean-pgj.R"))
# Clean SESNSP data
lastGood = as.Date(as.yearmon("2019-01-01"), frac = 1)
source(file.path("src", "pgj.R"))


start_date <-  "2018-02-01"
end_date <- "2019-02-01"

source(file.path("src", "gam.R"))
source(file.path("src", "pointdensity.R"))
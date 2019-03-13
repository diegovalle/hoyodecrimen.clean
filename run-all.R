## last date for SESNSP data
lastGood = as.Date(as.yearmon("2019-01-31"), frac = 1)
## dates for the GAM plot (should be last twelve months)
start_date <-  "2018-03-01"
end_date <- "2019-03-01"

start_points <- "2016-03-01"
end_points <- "2019-03-01"

# Clean PGJ-CDMX data
source(file.path("src", "packages.R"))
source(file.path("src", "clean-pgj.R"))
# Clean SESNSP data
source(file.path("src", "sesnsp.R"))



source(file.path("src", "gam.R"))
source(file.path("src", "pointdensity.R"))

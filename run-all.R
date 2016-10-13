
source(file.path("src", "packages.R"))

lastGood = as.Date(as.yearmon("2016-09-01"), frac = 1)

#source(file.path("src", "clean.R"))
source(file.path("src", "clean-daily.R"))
source(file.path("src", "map-clean.R"))
#source(file.path("src", "graphs.R"))
#source(file.path("src", "json.R"))

# unique(mcrime$municipio)
# muns <- fromJSON("https://hoyodecrimen.com/api/v1/municipios")$rows
# muns <- data.frame(municipio = unique(muns$municipio),
#                    cve_mun = unique(muns$cve_mun))
# write.csv(muns, "municipios.csv", row.names = FALSE)



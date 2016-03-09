
lastGood = "2016-01-01"


source(file.path("src", "packages.R"))
source(file.path("src", "clean.R"))
source(file.path("src", "map-clean.R"))
#source(file.path("src", "graphs.R"))
#source(file.path("src", "json.R"))

# unique(mcrime$municipio)
# muns <- fromJSON("https://hoyodecrimen.com/api/v1/municipios")$rows
# muns <- data.frame(municipio = unique(muns$municipio),
#                    cve_mun = unique(muns$cve_mun))
# write.csv(muns, "municipios.csv", row.names = FALSE)



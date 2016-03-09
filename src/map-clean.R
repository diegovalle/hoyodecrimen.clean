cuadrantes <- readOGR(file.path("shps_2016", "cuadrantes_population.shp"), 
                      layer = "cuadrantes_population")
names(cuadrantes@data) <- c("municipio", "zona", "cve_zona", "no_region", "sector", "sector2", 
                            "sector_hoyodecrimen",
                            "cve_sector", "no_cuadrante", "id", "x", "y", "SUMPOB1")
cuadrantes@data$x <- NULL
cuadrantes@data$y <- NULL
fcuadrantes <- fortify(cuadrantes, region = "id")

sectores <- readOGR(file.path("shps_2016", "sectores.shp"), layer = "sectores")
fsector <- fortify(sectores, region = "sector")

expect_equal(c(setdiff(unique(cuadrantes@data$id), unique(mcrime$cuadrante)),
             setdiff(unique(mcrime$cuadrante), unique(cuadrantes@data$id))),
             "(NO ESPECIFICADO)")



mcrime <- local({
  pop <- cuadrantes@data
  
  mcrime <- merge(mcrime, pop, by.x = "cuadrante", by.y = "id", all.x = TRUE)
  names(mcrime) <- c("cuadrante", "crime", "date", "count", "year", "municipio", 
                     "zona", "cve_zona", "no_region", "sector", "sector2", 
                     "sector_hoyodecrimen",
                     "cve_sector", 
                     "no_cuadrante", "population")
  
  muns <- read.csv(file.path("clean-data", "municipios.csv"))
  mcrime <- merge(mcrime, muns, all.x = TRUE)
  mcrime <- mcrime[order(mcrime$cuadrante, mcrime$crime, mcrime$date),] 
  write.csv(mcrime, file.path("clean-data", "cuadrantes-hoyodecrimen.csv"), row.names = FALSE)
  zip(file.path("clean-data", "cuadrantes.csv.zip"), 
      file.path("clean-data", "cuadrantes-hoyodecrimen.csv"))
  
  mcrime2 <- mcrime[ ,c("cuadrante", "crime", "date", "count", "year",
                        "sector_hoyodecrimen", "population")]
  names(mcrime2) <- c("cuadrante", "crime", "date", "count", "year",
                      "sector", "population")
  mcrime2$population[which(is.na(mcrime2$population) & 
                             mcrime2$cuadrante != "NO ESPECIFICADO")] <- NA
  mcrime2$sector <- as.character(mcrime2$sector)
  mcrime2$sector[is.na(mcrime2$sector)]  <- "NO ESPECIFICADO"
  mcrime2$date <- as.Date(mcrime2$date)
  #mcrime2 <- subset(mcrime2, date <= lastGood)
  mcrime2 <- mcrime2[order(mcrime2$cuadrante, mcrime2$crime, mcrime2$date),] 
  write.csv(mcrime2, file.path("clean-data", "cuadrantes.csv"), row.names = FALSE)
  mcrime
})

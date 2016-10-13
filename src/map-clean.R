cuadrantes <- readOGR(file.path("shps_2016", "cuadrantes_population.shp"), 
                      layer = "cuadrantes_population",
                      stringsAsFactors=FALSE,
                      encoding = "latin1", use_iconv=TRUE)
cuadrantes@data[which(cuadrantes@data[ ,"Sector_hoy"] == "TAXQUEA"), "Sector_hoy"] <- "TAXQUEÑA"
cuadrantes@data[which(cuadrantes@data[ ,"Sector"] == "TAXQUEA"), "Sector"] <- "TAXQUEÑA"
cuadrantes@data[which(cuadrantes@data[ ,"Sector2"] == "TAXQUEA"), "Sector2"] <- "TAXQUEÑA"

expect_equal(unique(cuadrantes@data$Sector_hoy),
             c("SAN ANGEL", "TEOTONGO", "TLATELOLCO", "BUENAVISTA", "MIXCALCO-HERALDO", 
               "REVOLUCION-ALAMEDA", "ANGEL-ZONA ROSA", "ROMA", "CONGRESO", 
               "CUAUTEPEC", "CUCHILLA", "IZTACCIHUATL", "CONSULADO", "MERCED-BALBUENA", 
               "MOCTEZUMA", "QUIROGA", "TEPEYAC", "TICOMAN", "ZARAGOZA", "LINDAVISTA", 
               "TLACOTAL", "PANTITLAN", "ARENAL", "PRADERA", "ASTURIAS", "ZAPOTITLA", 
               "UNIVERSIDAD", "CHURUBUSCO", "ABASTO-REFORMA", "ESTRELLA", "TEZONCO", 
               "QUETZAL", "NARVARTE-ALAMOS", "COAPA", "COYOACAN", "CULHUACAN", 
               "DEL VALLE", "NATIVITAS", "PORTALES", "NAPOLES", "TAXQUEÑA", "XOTEPINGO", 
               "TACUBA", "SOTELO", "CHAPULTEPEC", "POLANCO-CASTILLO", "TACUBAYA", 
               "HORMIGA", "CLAVERIA", "CUITLAHUAC", "LA RAZA", "MILPA ALTA", 
               "MIXQUIC", "HUIPULCO-HOSPITALES", "SAN JERONIMO", "FUENTE", "SANTA CRUZ", 
               "OASIS", "TECOMITL", "LA NORIA", "SANTA FE", "GRANJAS", "PLATEROS", 
               "DINAMO", "ARAGON", "ALPES", "CORREDOR-CENTRO", "MORELOS", "EL YAQUI", 
               "PADIERNA", "TEPEPAN", "CUAJIMALPA"))

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
                     "zona", "cve_zona", "no_region", "sector1", "sector2", 
                     "sector",
                     "cve_sector", 
                     "no_cuadrante", "population")
  
  muns <- read.csv(file.path("clean-data", "municipio_codes.csv"))
  mcrime <- merge(mcrime, muns, all.x = TRUE)
  mcrime <- mcrime[order(mcrime$cuadrante, mcrime$crime, mcrime$date),] 
  write.csv(mcrime, file.path("clean-data", "cuadrantes-hoyodecrimen.csv"), row.names = FALSE)
  zip(zipfile = file.path("clean-data", "cuadrantes.csv.zip"), 
      files = file.path("clean-data", "cuadrantes-hoyodecrimen.csv"))
  
  mcrime2 <- mcrime[ ,c("cuadrante", "crime", "date", "count", "year",
                        "sector", "population")]
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

local({
  municipios <- na.omit(mcrime[,c("cuadrante","sector","cve_mun","municipio")])
  names(municipios)[3] <- "cvegeo"
  municipios <- unique(municipios)
  write.csv(municipios, "clean-data/municipios.csv",  row.names = FALSE)
})
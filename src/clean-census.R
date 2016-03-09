census <- local({
  pop <- read.dbf(file.path("data", "cuadrantes-sspdf-census-data.dbf"))
  
  eco <- read.dbf(file.path("data", "df_cpv2010_manzanas_caracteristicas_economicas.dbf"))
  edu <- read.dbf(file.path("data", "df_cpv2010_manzanas_caracteristicas_educativas.dbf"))
  fec <- read.dbf(file.path("data", "df_cpv2010_manzanas_fecundidad.dbf"))
  hog <- read.dbf(file.path("data", "df_cpv2010_manzanas_hogares_censales.dbf"))
  mig <- read.dbf(file.path("data", "df_cpv2010_manzanas_migracion.dbf"))
  sc <- read.dbf(file.path("data", "df_cpv2010_manzanas_situacion_conyugal.dbf"))
  viv <- read.dbf(file.path("data", "df_cpv2010_manzanas_viviendas.dbf"))
  
  # POB1  Población total
  # POB13  Población de 18 a 24 años
  # POB14	Población de 30 a 49 años
  # POB31  Población femenina
  # POB69  Población masculina de 18 a 24 años
  # POB70	Población masculina de 30 a 49 años
  pop <- pop[,c("CVEGEO", "POB1", "POB13", "POB14", "POB31", "POB69", "POB70", "id", "sector")]
  # MIG1  Población nacida en la entidad
  # MIG7  Población nacida en otro país
  # MIG15  Población de 5 años y más residente en Estados Unidos de América en junio de 2005
  mig <- mig[,c("CVEGEO", "MIG1","MIG7","MIG15" )]
  # EDU25  Población de 15 años y más alfabeta
  # EDU31  Población de 15 años y más sin escolaridad
  # EDU46  Población de 25 años y más con al menos un grado aprobado en educación superior
  edu <- edu[,c("CVEGEO", "EDU25","EDU31" ,"EDU46")]
  # ECO1  Población económicamente activa
  # ECO27  Población masculina desocupada
  eco <- eco[,c("CVEGEO", "ECO1","ECO27")]
  # SCONY1  Población soltera o nunca unida de 12 años y más
  # SCONY10  Población casada o unida de 15 a 24 años
  sc <- sc[,c("CVEGEO", "SCONY1", "SCONY10"),]
  # HOGAR1  Total de hogares censales
  # HOGAR5  Población en hogares censales con jefatura femenina
  # HOGAR26  Población en hogares censales nucleares conformados por la jefa con hijos menores de 18 años
  hog <- hog[,c("CVEGEO", "HOGAR1","HOGAR5","HOGAR26")]
  # VIV0  Total de viviendas
  # VIV1  Total de viviendas habitadas
  #   VIV2  Viviendas particulares habitadas
  # VIV3  Ocupantes en viviendas particulares
  # VIV2_R  Porcentaje de viviendas particulares habitadas
  # VIV18_R  Porcentaje de ocupantes en viviendas particulares con acceso a agua entubada en el ámbito de la vivienda
  # VIV21_R  Porcentaje de ocupantes en viviendas particulares que disponen de excusado con admisión de agua y drenaje
  # VIV9_R  Porcentaje de viviendas particulares habitadas con más de 2.5 ocupantes por dormitorio
  # VIV6_R  Porcentaje de viviendas particulares habitadas con piso de tierra
  # VIV39_R  Porcentaje de viviendas particulares habitadas sin computadora ni Internet
  viv <- viv[,c("CVEGEO", "VIV0","VIV1","VIV2", "VIV3","VIV2_R",
                "VIV18_R","VIV21_R","VIV9_R","VIV6_R","VIV39_R",
                "VIV18", "VIV21", "VIV9", "VIV6", "VIV39")]
  # FEC1_R  Promedio de hijos nacidos vivos
  # FEC3_R  Porcentaje de mujeres de 15 a 19 años con al menos un hijo nacido vivo
  fec <- fec[,c("CVEGEO", "FEC1_R","FEC3_R")]
  
  census <- Reduce(function(x,y) { merge(x,y, all = TRUE, by = "CVEGEO") },
                   list(pop, mig, edu, eco, sc, hog, viv, fec))
  return(census)
})

#unique(census[,"MIG7"])
## Get rid of the negative numbers
census[,-c(1:9)] <- as.data.frame(apply(census[,-c(1:9)], 2, 
                                        function(x) {ifelse(x < 0, 0, as.numeric(x))}))
ddply(census, .(id,), summarise,
      )

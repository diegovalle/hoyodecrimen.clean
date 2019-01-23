
url <- "https://datos.cdmx.gob.mx/explore/dataset/carpetas-de-investigacion-pgj-cdmx/download/?format=csv&timezone=America/Mexico_City&use_labels_for_header=true"
df <- read.csv(url, sep = ";")


df$id <- 1:nrow(df)
df$Latitud <- as.numeric(df$Latitud)
df$Longitud <- as.numeric(df$Longitud)
unique(df$Categoría.de.delito)
df <- filter(df, Categoría.de.delito %in% c( 
  "HOMICIDIO DOLOSO", 
  "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
  "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
  "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA", 
  "ROBO A NEGOCIO CON VIOLENCIA", 
  "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA", 
  "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA", 
  "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
  "ROBO A REPARTIDOR CON Y SIN VIOLENCIA", 
  "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA", 
  "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA", 
  "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  "SECUESTRO", 
  "VIOLACIÓN"))


## To which cuadrante do the lat + long belong
cuad_map <- readOGR(file.path("shps_2016", "cuadrantes_population.shp"), 
        layer = "cuadrantes_population",
        stringsAsFactors=FALSE,
        encoding = "latin1", use_iconv=TRUE)
cuad_map@data[which(cuad_map@data[ ,"Sector_hoy"] == "TAXQUEA"), "Sector_hoy"] <- "TAXQUEÑA"
cuad_map@data[which(cuad_map@data[ ,"Sector"] == "TAXQUEA"), "Sector"] <- "TAXQUEÑA"
cuad_map@data[which(cuad_map@data[ ,"Sector2"] == "TAXQUEA"), "Sector2"] <- "TAXQUEÑA"
expect_equal(unique(cuad_map@data$Sector_hoy),
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
ID <- filter(df, !is.na(df$Longitud) | !is.na(df$Latitud))
coordinates(ID) <- ~ Longitud + Latitud
proj4string(ID) <- CRS("+proj=longlat +datum=WGS84")
ID <-spTransform(ID, proj4string(cuad_map))
df <- left_join(df,
          cbind(as.data.frame(ID)[, "id", drop = FALSE], 
                sp::over(ID, cuad_map, returnList = FALSE)[, c("Nomenclatu", "Sector_hoy", "SUMPOB1")]),
          by = "id"
)
df <- dplyr::rename(df, "cuadrante" = "Nomenclatu", "sector" = "Sector_hoy", "population" = "SUMPOB1")
df[is.na(df$cuadrante), "cuadrante"] <- "(NO ESPECIFICADO)"
df[is.na(df$sector), "sector"] <- "NO ESPECIFICADO"

a=df %>%
  #filter(Categoría.de.delito == "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") %>%
  group_by(Delito, Categoría.de.delito ) %>%
  summarise(n = n())

df$crime <- df$Categoría.de.delito
unique(str_subset(df$crime, "CON Y SIN"))

df$crime[str_detect(df$crime, "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") &
  str_detect(df$Delito, "CON")] <- "ROBO DE VEHÍCULO CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO DE VEHÍCULO SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A PASAJERO A BORDO DEL METRO CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A PASAJERO A BORDO DEL METRO SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A PASAJERO A BORDO DE MICROBUS CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A PASAJERO A BORDO DE MICROBUS SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A REPARTIDOR CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A REPARTIDOR CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A REPARTIDOR CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A REPARTIDOR SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A TRANSEUNTE EN VÍA PÚBLICA SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A TRANSPORTISTA CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A TRANSPORTISTA SIN VIOLENCIA"


test_that("", {
  expect_equal(length(str_subset(df$crime, "CON Y SIN")), 0)
})

df$crime <- str_replace_all(df$crime, "CON VIOLENCIA", "C.V.")
df$crime <- str_replace_all(df$crime, "SIN VIOLENCIA", "S.V.")
df$crime <- str_replace_all(df$crime, 
                            "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
                            "LESIONES POR ARMA DE FUEGO")
df$crime <- str_replace_all(df$crime, 
                            "ROBO A PASAJERO A BORDO DEL METRO C.V.", 
                            "ROBO A BORDO DE METRO C.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO A PASAJERO A BORDO DEL METRO S.V.", 
                            "ROBO A BORDO DE METRO S.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO A PASAJERO A BORDO DE MICROBUS C.V.", 
                            "ROBO A BORDO DE MICROBUS C.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO A PASAJERO A BORDO DE MICROBUS S.V.", 
                            "ROBO A BORDO DE MICROBUS S.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO C.V.", 
                            "ROBO A CUENTAHABIENTE C.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO A TRANSEUNTE EN VÍA PÚBLICA C.V.", 
                            "ROBO A TRANSEUNTE C.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO A TRANSEUNTE EN VÍA PÚBLICA S.V.", 
                            "ROBO A TRANSEUNTE S.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO DE VEHÍCULO C.V.", 
                            "ROBO DE VEHICULO AUTOMOTOR C.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO DE VEHÍCULO S.V.", 
                            "ROBO DE VEHICULO AUTOMOTOR S.V.")
df$crime <- str_replace_all(df$crime, 
                            "ROBO A PASAJERO A BORDO DE TAXI C.V.", 
                            "ROBO A BORDO DE TAXI C.V.")


df %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  group_by(Año, Categoría.de.delito) %>%
  summarise(n = n())

df %>%
  group_by(crime) %>%
  summarise(n = n()) %>%
  arrange(n)

### File for the database

cuadrantes <- df
cuadrantes$date <- df$Mes.y.año
cuadrantes <- cuadrantes %>%
  group_by(date, crime, cuadrante) %>%
  summarise(count = n()) %>%
  ungroup()
cuadrantes <-  full_join(cuadrantes, 
                         expand(cuadrantes, date, cuadrante, crime),
                         by = c("cuadrante", "crime", "date"))
cuadrantes <-  left_join(cuadrantes, 
                         unique(df[, c("cuadrante", "sector", "population")]),
                         by = "cuadrante")
cuadrantes <-   cuadrantes %>%
  mutate(date = str_c(date, "-01")) %>%
  mutate(year = str_sub(date, 1, 4)) %>%
  arrange(cuadrante, crime, date)
cuadrantes$count[is.na(cuadrantes$count)] <- 0
cuadrantes <- cuadrantes[ ,c("cuadrante", "crime", "date", "count", "year", 
                      "sector", "population")]
write.csv(cuadrantes, file.path("clean-data", "cuadrantes-pgj.csv"), row.names = FALSE)


### File for the cartodb map visualization


df %>%
  mutate(date = fast_strptime(str_replace(df$Fecha.inicio, "0([5|6]):00$", "0\\100"), 
                              format = "%Y-%m-%dT%H:%M:%S%z",
                              lt = FALSE)) %>%
  mutate(hour = hour(date)) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(date = format(date, "%Y-%m-%d")) %>%
  rename("lat" = "Latitud", "long" = "Longitud") %>%
  select(cuadrante, crime, date, hour, year, month, lat, long, id) %>%
  write_csv("clean-data/crime-lat-long-pgj.csv")

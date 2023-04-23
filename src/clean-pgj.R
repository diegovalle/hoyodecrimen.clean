print("Cleaning PGJ-CDMX data")
## File available at:
# https://datos.cdmx.gob.mx/dataset/carpetas-de-investigacion-fgj-de-la-ciudad-de-mexico

page <- readLines("https://datos.cdmx.gob.mx/dataset/carpetas-de-investigacion-fgj-de-la-ciudad-de-mexico",
                  warn = FALSE)
page <- paste0(page, collapse = "")
url <- str_extract_all(page, '(?<=href=")https://archivo.datos.cdmx.gob.mx/fiscalia-general-de-justicia/carpetas-de-investigacion-fgj-de-la-ciudad-de-mexico/carpetas_[a-zA-Z_0-9]*\\.csv(?=")')

tmp <- tempfile()
download.file(destfile = tmp, url = url[[1]][[length(url[[1]])]])

carpetas <- read_csv(tmp, col_types = cols(
  ao_hechos = col_character(),
  mes_hechos = col_character(),
  fecha_hechos = col_date(format = ""),
  hora_hechos = col_time(format = ""),
  ao_inicio = col_character(),
  mes_inicio = col_character(),
  fecha_inicio = col_date(format = ""),
  hora_inicio = col_time(format = ""),
  delito = col_character(),
  fiscalia = col_character(),
  agencia = col_character(),
  unidad_investigacion = col_character(),
  categoria_delito = col_character(),
  competencia = col_character(),
  alcaldia_hechos = col_character(),
  municipio_hechos = col_character(),
  colonia_datos = col_character(),
  fgj_colonia_registro = col_character(),
  longitud = col_double(),
  latitud = col_double()
))

df <- bind_rows(read_csv("https://archivo.datos.cdmx.gob.mx/fiscalia-general-de-justicia/carpetas-de-investigacion-fgj-de-la-ciudad-de-mexico/carpetas_2019-2021.csv", 
                         col_types = cols(
                           ao_hechos = col_character(),
                           mes_hechos = col_character(),
                           fecha_hechos = col_date(format = ""),
                           hora_hechos = col_time(format = ""),
                           ao_inicio = col_character(),
                           mes_inicio = col_character(),
                           fecha_inicio = col_date(format = ""),
                           hora_inicio = col_time(format = ""),
                           delito = col_character(),
                           fiscalia = col_character(),
                           agencia = col_character(),
                           unidad_investigacion = col_character(),
                           categoria_delito = col_character(),
                           competencia = col_character(),
                           alcaldia_hechos = col_character(),
                           municipio_hechos = col_character(),
                           colonia_datos = col_character(),
                           fgj_colonia_registro = col_character(),
                           longitud = col_double(),
                           latitud = col_double()
                         )),
                carpetas)

# rename columns to standardize names
df <- df %>% rename(Latitud = latitud,
                    Longitud = longitud,
              Categoría.de.delito = categoria_delito,
              fecha_hechos = fecha_hechos,
              Año = ao_hechos,
              Mes = mes_hechos,
              Delito = delito)
df$Año <- year(df$fecha_hechos)

df$fecha_hechos <- paste(df$fecha_hechos, df$hora_hechos)

df$id <- 1:nrow(df)
df$Latitud <- as.numeric(df$Latitud)
df$Longitud <- as.numeric(df$Longitud)
unique(df$Categoría.de.delito)
df$Categoría.de.delito <- str_replace_all(df$Categoría.de.delito, 
                c("ROBO A TRANSEUNTE EN V\u0099A P\u0082BLICA CON Y SIN VIOLENCIA" =
                    "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA" , 
                  "ROBO A CASA HABITACI\u0085N CON VIOLENCIA" =
                    "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
                  "ROBO DE VEH\u0099CULO CON Y SIN VIOLENCIA" =
                    "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" , 
                  "VIOLACI\u0085N" =
                    "VIOLACIÓN" , 
                  "ROBO DE VEHÖCULO CON Y SIN VIOLENCIA" =
                    "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" , 
                  "VIOLACIàN" =
                    "VIOLACIÓN" ,
                  "ROBO A CASA HABITACIàN CON VIOLENCIA" =
                    "ROBO A CASA HABITACIÓN CON VIOLENCIA" ,
                  "ROBO A TRANSEUNTE EN VÖA PéBLICA CON Y SIN VIOLENCIA" =
                    "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA"))
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
suppressWarnings(
  cuad_map <- readOGR(file.path("shps_2016", "cuadrantes_population.shp"),
                      layer = "cuadrantes_population",
                      stringsAsFactors = FALSE,
                      encoding = "latin1",
                      use_iconv = TRUE,
                      verbose = FALSE)
)
cuad_map@data[which(cuad_map@data[, "Sector_hoy"] == "TAXQUEA"),
              "Sector_hoy"] <- "TAXQUEÑA"
cuad_map@data[which(cuad_map@data[, "Sector"] == "TAXQUEA"),
              "Sector"] <- "TAXQUEÑA"
cuad_map@data[which(cuad_map@data[, "Sector2"] == "TAXQUEA"),
              "Sector2"] <- "TAXQUEÑA"
expect_equal(unique(cuad_map@data$Sector_hoy),
             c("SAN ANGEL", "TEOTONGO", "TLATELOLCO",
               "BUENAVISTA", "MIXCALCO-HERALDO",
               "REVOLUCION-ALAMEDA", "ANGEL-ZONA ROSA", "ROMA", "CONGRESO",
               "CUAUTEPEC", "CUCHILLA", "IZTACCIHUATL",
               "CONSULADO", "MERCED-BALBUENA",
               "MOCTEZUMA", "QUIROGA", "TEPEYAC", "TICOMAN",
               "ZARAGOZA", "LINDAVISTA",
               "TLACOTAL", "PANTITLAN", "ARENAL", "PRADERA",
               "ASTURIAS", "ZAPOTITLA",
               "UNIVERSIDAD", "CHURUBUSCO", "ABASTO-REFORMA",
               "ESTRELLA", "TEZONCO",
               "QUETZAL", "NARVARTE-ALAMOS", "COAPA", "COYOACAN",
               "CULHUACAN",
               "DEL VALLE", "NATIVITAS", "PORTALES", "NAPOLES",
               "TAXQUEÑA", "XOTEPINGO",
               "TACUBA", "SOTELO", "CHAPULTEPEC",
               "POLANCO-CASTILLO", "TACUBAYA",
               "HORMIGA", "CLAVERIA", "CUITLAHUAC",
               "LA RAZA", "MILPA ALTA",
               "MIXQUIC", "HUIPULCO-HOSPITALES",
               "SAN JERONIMO", "FUENTE", "SANTA CRUZ",
               "OASIS", "TECOMITL", "LA NORIA", "SANTA FE",
               "GRANJAS", "PLATEROS",
               "DINAMO", "ARAGON", "ALPES", "CORREDOR-CENTRO",
               "MORELOS", "EL YAQUI",
               "PADIERNA", "TEPEPAN", "CUAJIMALPA"))
ID <- filter(df, !is.na(df$Longitud))
ID <- filter(ID, !is.na(ID$Latitud))
coordinates(ID) <- ~ Longitud + Latitud
proj4string(ID) <- CRS("+proj=longlat +datum=WGS84")
ID <- spTransform(ID, proj4string(cuad_map))
df <- left_join(df,
          cbind(as.data.frame(ID)[, "id", drop = FALSE],
                sp::over(ID, cuad_map, returnList = FALSE)[,
                                                           c("Nomenclatu",
                                                             "Sector_hoy",
                                                             "SUMPOB1")]),
          by = "id"
)
df <- dplyr::rename(df,
                    "cuadrante" = "Nomenclatu",
                    "sector" = "Sector_hoy",
                    "population" = "SUMPOB1")
df[is.na(df$cuadrante), "cuadrante"] <- "(NO ESPECIFICADO)"
df[is.na(df$sector), "sector"] <- "NO ESPECIFICADO"

df %>%
  #filter(Categoría.de.delito == "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") %>%
  group_by(Delito, Categoría.de.delito ) %>%
  summarise(n = n())

df$crime <- df$Categoría.de.delito
unique(str_subset(df$crime, "CON Y SIN"))

df$crime[str_detect(df$crime, "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") &
  str_detect(df$Delito, "CON|C/V")] <- "ROBO DE VEHÍCULO CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN|S/V")] <- "ROBO DE VEHÍCULO SIN VIOLENCIA"

df$crime[str_detect(df$crime,
                    "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <-
  "ROBO A PASAJERO A BORDO DEL METRO CON VIOLENCIA"
df$crime[str_detect(df$crime,
                    "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <-
  "ROBO A PASAJERO A BORDO DEL METRO SIN VIOLENCIA"

df$crime[str_detect(df$crime,
                    "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <-
  "ROBO A PASAJERO A BORDO DE MICROBUS CON VIOLENCIA"
df$crime[str_detect(df$crime,
                    "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <-
  "ROBO A PASAJERO A BORDO DE MICROBUS SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A REPARTIDOR CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A REPARTIDOR CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A REPARTIDOR CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A REPARTIDOR SIN VIOLENCIA"

df$crime[str_detect(df$crime,
                    "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <-
  "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON VIOLENCIA"
df$crime[str_detect(df$crime,
                    "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <-
  "ROBO A TRANSEUNTE EN VÍA PÚBLICA SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A TRANSPORTISTA CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "C/V")] <- "ROBO A TRANSPORTISTA CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A TRANSPORTISTA SIN VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "S/V")] <- "ROBO A TRANSPORTISTA SIN VIOLENCIA"



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


# remove accents
df$crime <- iconv(df$crime, from = "UTF-8", to = "ASCII//TRANSLIT")

df$Año <- as.numeric(df$Año)
df %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  group_by(Año, Mes, Categoría.de.delito) %>%
  summarise(n = n()) %>%
  arrange(desc(Año))

df %>%
  group_by(crime) %>%
  summarise(n = n()) %>%
  arrange(n)

## Correct errors in the data

df <- df[!is.na(df$fecha_hechos), ]
df <- filter(df, Año >= 2019)

df$fecha_hechos <- str_replace_all(df$fecha_hechos, 
                                   "^(\\d{2})/(\\d{2})/(\\d{4})", 
                                   "\\3-\\2-\\1")
# df$fecha_hechos <- str_replace_all(df$fecha_hechos, 
#                                    "^(\\d{4}-\\d{2}-\\d{2})T", 
#                                    "\\1 ")

#validate date format
#which(!str_detect(df$fecha_hechos, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:?\\d{0,2}"))
df$fecha_hechos <- str_replace_all(df$fecha_hechos, "T|Z", " ")
#df$fecha_hechos <- str_c(df$fecha_hechos, " ", df$HoraHecho)
df <- df[!is.na(df$fecha_hechos), ]
#df$fecha_hechos <- as.character(parse_date_time(df$fecha_hechos, "ymd HMS"))
expect_true(all(str_detect(df$fecha_hechos,
                           "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:?\\d{0,2}")))

# add missing seconds to fecha_hechos
df$fecha_hechos <- if_else(str_length(df$fecha_hechos) == 16, 
        paste0(df$fecha_hechos, ":00"), 
        df$fecha_hechos)

df$fecha_hechos2 <- parse_date_time(df$fecha_hechos, "Ymd H:M:S")

expect_true(all(str_detect(df$fecha_hechos2,
                           "\\d{4}-\\d{2}-\\d{2} \\d{1,2}:\\d{2}:\\d{2}")))
expect_true(max(df$fecha_hechos2) < Sys.Date())

expect_identical(df$Año, year(df$fecha_hechos2))
df <- filter(df, Mes != "data")
expect_equal(as.character(month(df$fecha_hechos2, label = TRUE, abbr = TRUE)),
             str_replace_all(df$Mes,
                             c("Enero" = "Jan", "Febrero" = "Feb", "Marzo" = "Mar",
                               "Abril" = "Apr", "Mayo" = "May", "Junio" = "Jun",
                               "Julio" = "Jul", "Agosto" = "Aug", "Septiembre" = "Sep",
                               "Octubre" = "Oct", "Noviembre" = "Nov", "Diciembre" = "Dec"))
)

df$Mes.y.año <- format(as.Date(df$fecha_hechos2), "%Y-%m")
expect_true(all(str_detect(df$Mes.y.año, "\\d{4}-\\d{2}")))
#df$fecha_hechos2 <- df$fecha_hechos2 %>% 
#  with_tz("America/Mexico_City")

### File for the database

cuadrantes <- df
cuadrantes$date <- df$Mes.y.año
cuadrantes <- cuadrantes %>%
  group_by(date, crime, cuadrante) %>%
  summarise(count = n()) %>%
  ungroup()
cuadrantes <-  full_join(cuadrantes,
                         tidyr::expand(cuadrantes, date,
                                       cuad_map@data$Nomenclatu, crime),
                         by = c("cuadrante" = "cuad_map@data$Nomenclatu",
                                "crime", "date"))
cuadrantes <-  left_join(cuadrantes,
                         unique(cuad_map@data[, c("Nomenclatu",
                                                  "Sector_hoy",
                                                  "SUMPOB1")]),
                         by = c("cuadrante" = "Nomenclatu")) %>%
  rename("sector" = "Sector_hoy", "population" = "SUMPOB1")
cuadrantes <-   cuadrantes %>%
  mutate(date = str_c(date, "-01")) %>%
  mutate(year = str_sub(date, 1, 4)) %>%
  filter(year >= 2016) %>%
  arrange(cuadrante, crime, date)
cuadrantes$count[is.na(cuadrantes$count)] <- 0
cuadrantes$sector[is.na(cuadrantes$sector)] <- "NO ESPECIFICADO"
cuadrantes <- cuadrantes[, c("cuadrante", "crime", "date", "count", "year",
                      "sector", "population")]
write.csv(cuadrantes, file.path("clean-data", "cuadrantes-pgj.csv"),
          row.names = FALSE)


### File for the cartodb map visualization


df %>%
  mutate(date = fast_strptime(as.character(df$fecha_hechos2),
                              format = "%Y-%m-%d %H:%M:%S",
                              lt = FALSE)) %>%
  mutate(date = force_tz(date, "America/Mexico_City")) %>%
  mutate(hour = as.character(format(date, "%H:%M"))) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(date = as.character(format(date, "%Y-%m-%d"))) %>%
  rename("lat" = "Latitud", "long" = "Longitud") %>%
  select(cuadrante, crime, date, hour, year, month, lat, long, id) %>%
  write.csv("clean-data/crime-lat-long-pgj.csv", row.names = FALSE)

## Sometimes there are problems with the timezone in the data
## check that most homicides occur at ~midnight
df %>%
  mutate(hour = as.numeric(str_sub(fecha_hechos2, 11, 13))) %>%
  filter(crime == "ROBO A BORDO DE METRO C.V.") %>% #ROBO A BORDO DE METRO C.V.
  group_by(hour) %>%
  summarise(n = n()) %>%
  ggplot(aes(hour, n)) +
  geom_col() +
  ggtitle("Robo de Metro - Most homicides should occur at midnight or for robo de metro none during late night")
ggsave(filename = "graphs/check.png", width = 7, height = 5, dpi = 100)

cuadrantes %>%
  group_by(date, crime) %>%
  summarise(n = sum(count)) %>%
  ggplot(aes(as.Date(date), n)) +
    geom_line() +
    xlab("fecha") +
    ylab("número de crímenes") +
    expand_limits(y = 0) +
    labs(title = "Crimen en la Ciudad de México") +
    facet_wrap(~ crime, scale = "free_y")
ggsave("graphs/crimes.png", dpi = 100, width = 14, height = 7)


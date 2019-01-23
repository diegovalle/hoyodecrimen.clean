fileName <- "2016.xlsx"
df16 <- read_excel(file.path("sspdf-data", fileName), range = "A36:L23041")
# we need to transform the corrdinates from NAD83 14N to WGS84
xy <- df[,c("COORDX", "COORDY")]
spdf <- SpatialPointsDataFrame(coords = xy, data = df[,c("FECHA", "DELITO", "MES", "AÑO", "Coordinación Territorial de Hechos", 
                                                         "CALLE1", "CALLE2", "COLONIA", "DELEGACION", "CUADRANTE")],
                               proj4string = CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
spdf <- spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +no_defs"))
df16 <- as.data.frame(spdf)

df16$COORDX[df16$COORDX == 0] <- NA
df16$COORDX[df16$COORDY == 0] <- NA
df16$COORDY[df16$COORDY == 0] <- NA
df16$COORDY[df16$COORDY < 5] <- NA
df16$COORDX[is.na(df16$COORDY)] <- NA
df16 <- subset(df16, AÑO == 2013)

head(df16)
names(df16) <- c("date", "hour", "street1", "street2", "colonia", "delegacion", "cuadrante",
               "crime", "month", "year", "long", "lat")

df16 <- df16[ , c("cuadrante", "crime", "date", "hour", "year", "month", "lat", "long")]
df16$date <- ymd(df16$date)
df16$month <- month(df16$date)





fileName <- "2013.csv"
df <- read.csv(file.path("sspdf-data", fileName)) #, range = "A20:L65536"

# we need to transform the corrdinates from NAD83 14N to WGS84
xy <- df[,c("COORDX", "COORDY")]
spdf <- SpatialPointsDataFrame(coords = xy, data = df[,c("FECHA", "HORA", "CALLE1", "CALLE2", "COLONIA", "DELEGACION", 
                                                         "CUADRANTE", "DELITO", "MES", "AÑO")],
                               proj4string = CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
spdf <- spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +no_defs"))
df <- as.data.frame(spdf)


df$COORDX[df$COORDX == 0] <- NA
df$COORDX[df$COORDY == 0] <- NA
df$COORDY[df$COORDY == 0] <- NA
df$COORDY[df$COORDY < 5] <- NA
df$COORDX[is.na(df$COORDY)] <- NA
df <- subset(df, AÑO == 2013)

head(df)
names(df) <- c("date", "hour", "street1", "street2", "colonia", "delegacion", "cuadrante",
               "crime", "month", "year", "long", "lat")

df13 <- df[ , c("cuadrante", "crime", "date", "hour", "year", "month", "lat", "long")]
df13$date <- ymd(df13$date)
df13$month <- month(df13$date)
#write.csv(df13, "sspdf-data/2013.csv", row.names = FALSE)

## Data later than 2013

fileName <- "2014-2016.xlsx"
df <- read_excel(file.path("sspdf-data", fileName), skip = 0)

names(df) <- c("crime", "year", "date", "hour", "cuadrante", "long", "lat", "month", "", "")
df <- df[ ,c("cuadrante", "crime", "date", "hour", "year", "month", "lat", "long")]

#remove all rows with only NAs
df <- df[rowSums(is.na(df)) < ncol(df),]
df$lat[df$lat < 18] <- NA
df$long[df$long > 350] <- NA
df$long[df$long > -98.884142] <- NA
df$long[is.na(df$lat)] <- NA
df$lat[is.na(df$long)] <- NA

df$date <- ymd(df$date)
df$month <- month(df$date)

df <- rbind(df13,df)

df$cuadrante[is.na(df$cuadrante)] <- ""
df$cuadrante <- str_replace(df$cuadrante, "^$", "(NO ESPECIFICADO)")

stopifnot(df$month[df$month == ""] == numeric(0))
stopifnot(df$cuadrante[df$cuadrante == ""] == numeric(0))

df <- subset(df, date <= lastGood)

df$crime <- iconv(df$crime, to='ASCII//TRANSLIT')
df$crime <- toupper(df$crime)
df$cuadrante <- toupper(df$cuadrante)

df$crime <- str_replace_all(df$crime, "C/V", "C.V.")
df$crime <- str_replace_all(df$crime, "S/V", "S.V.")


df$crime <- str_replace_all(df$crime, "ROBO DE VEHICULO C.V.", 
                            "ROBO DE VEHICULO AUTOMOTOR C.V.")
df$crime <- str_replace_all(df$crime, "ROBO DE VEHICULO S.V.", 
                            "ROBO DE VEHICULO AUTOMOTOR S.V.")
df$crime <- str_replace_all(df$crime, "ROBO A REPARTIDOR C.V.EH", 
                            "ROBO A REPARTIDOR C.V.")
df$crime <- str_replace_all(df$crime, "ROBO A CUENTAHABIENTE SALIENDO DE CAJERO Y/O SUC. BANCARIA C.V.",
                            "ROBO A CUENTAHABIENTE O SUC. BANCARIA C.V.")
df$id <- df$id <- 1:nrow(df)
stopifnot(length(unique(df$cuadrante)) == 847 | length(unique(df$cuadrante)) == 848)
write.csv(df, "clean-data/crime-lat-long.csv", row.names = FALSE)
df$id <- NULL


mcrime <- df %>%
  group_by(crime, cuadrante, year, month) %>%
  summarise(count = n()) %>%
  mutate(date = ymd(str_c(year, month, "1", sep = "-")))
mcrime <- mcrime[, c("crime", "cuadrante", "date", "count", "year")]

expand <- expand.grid(crime = unique(mcrime$crime),
                      date = unique(mcrime$date),
                      cuadrante = unique(mcrime$cuadrante))

mcrime <- right_join(mcrime, expand, by = c("crime", "cuadrante", "date"))

mcrime$year <- year(mcrime$date)
mcrime$count <- as.numeric(mcrime$count)
mcrime$count[is.na(mcrime$count)] <- 0
#df <- read.csv(file.path("sspdf-data", fileName), 
#         stringsAsFactors = FALSE, skip = 32)

# df %>% 
#   group_by(DELITO) %>%
#   summarise(count = n())
# 
# df$date <- parse_date_time(df$FECHAINICI, 
#                                 "%m/%d/%y")
# 
# df$date_time <- parse_date_time(str_c(df$FECHAINICI,
#                                       df$HORAINICIO), 
#                                 "%m/%d/%y %H!:%M!",
#                                 tz="America/Mexico_City")
# df$hour <- factor(hour(df$date_time), levels = c(6:23,0:5, NA))
# df$wday <- wday(df$date, label = TRUE, abbr = TRUE)
# 
# ggplot(df %>% 
#          group_by(DELITO, wday) %>%
#          summarise(count = n()),
#        aes(wday,count, group = DELITO)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~DELITO, scales = "free_y")
# 
# ggplot(df %>% 
#          group_by(DELITO, hour) %>%
#          summarise(count = n()),
#        aes(hour,count, group = DELITO)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~DELITO, scales = "free_y")

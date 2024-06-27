# For the frontpage map only homicides are needed
crime_lat_long <- read.csv("clean-data/crime-lat-long-pgj.csv")
end_date <- max(ymd(crime_lat_long$date), na.rm = TRUE)
start_date <- floor_date(end_date, "month") - years(1)
start_date <- ceiling_date(start_date, "month")


crime_lat_long <- filter(crime_lat_long, !is.na(long) & !is.na(lat)) %>%
  filter(date <= end_date & date >= start_date) %>%
  filter(crime == c("HOMICIDIO DOLOSO"))
# order has to be in lon/lat
xy <- round(crime_lat_long[, c("long", "lat")], 6)
spdf <- SpatialPointsDataFrame(coords = xy, 
                               data = select(crime_lat_long, -c(lat, long)),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
geojson_write(spdf, file = "clean-data/json/homicides.geojson")

# All crimes to be mapped
crime_lat_long <- read.csv("clean-data/crime-lat-long-pgj.csv")
end_date <- max(ymd(crime_lat_long$date), na.rm = TRUE)
start_date <- floor_date(end_date, "month") - years(1)
start_date <- ceiling_date(start_date, "month")

crime_lat_long <- filter(crime_lat_long, !is.na(long) & !is.na(lat)) %>%
  filter(date <= end_date & date >= start_date) %>%
  filter(crime %in% c("HOMICIDIO DOLOSO",
         "ROBO DE VEHICULO AUTOMOTOR C.V.",
         "ROBO DE VEHICULO AUTOMOTOR S.V.",
         "ROBO A TRANSEUNTE C.V.",
         "LESIONES POR ARMA DE FUEGO"))
# order has to be in lon/lat
xy <- round(crime_lat_long[, c("long", "lat")], 6)
spdf <- SpatialPointsDataFrame(coords = xy, 
                               data = select(crime_lat_long, -c(lat, long)),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
geojson_write(spdf, file = "clean-data/json/selected-crimes.geojson")

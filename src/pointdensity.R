print("Point density maps")

# Point density of homicides during the last year

density_last_year <- function(crime){
  points <- read.csv("clean-data/crime-lat-long-pgj.csv") %>%
    filter(date <= max(date, na.rm = TRUE) & 
             date > ceiling_date(
               floor_date(ymd(max(date, na.rm = TRUE)), "month") - years(1), 
               "month")
    ) %>%
    filter(crime == !!crime) %>%
    na.omit()
  SD_density <- pointdensity(df = points, lat_col = "lat", lon_col = "long",
                             date_col = "date", grid_size = .1, radius = 1)
  return(list("density" = SD_density, "start" = min(points$date),
              "end" = max(points$date, na.rm = TRUE)))
}

density_chart <- function(crime, ll) {
  ll$density <- subset(ll$density, lat != 0)
  ll$density <- subset(ll$density, lon != 0)
  density_title <- str_c("Point density of ",
                         crime,
                         " counts in CDMX with a 1 km",
                         " radius\n(",
                         format(as.Date(ll[["start"]]), "%b %Y"),
                         " - ",
                         format(as.Date(ll[["end"]]), "%b %Y"),
                         ")")
  qmplot(lon, lat, data = ll[["density"]], geom = "blank",
         zoom = 11, maptype = "stamen_terrain", darken = .4, legend = "topleft") +
    geom_point(aes(x = lon, y = lat, colour = count), shape = 19, size = 2,
               data = ll[["density"]]) +
    scale_color_viridis(option = "inferno") +
    labs(title = density_title) 
}

ll <- density_last_year("HOMICIDIO DOLOSO")
xy <- round(ll$density[, c("lon", "lat")], 6)
spdf <- SpatialPointsDataFrame(coords = xy, 
                               data = select(ll$density, -c(lat, lon)),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
geojson_write(spdf, file = "clean-data/json/homicides-density.geojson")
p <- density_chart("homicide", ll)
ggsave("graphs/density_homicides.png", 
       plot = p, dpi = 100, width = 7, height = 8)

ll <- density_last_year("ROBO DE VEHICULO AUTOMOTOR C.V.")
p <- density_chart("car robbery w/v", ll)
ggsave("graphs/density_car_robbery_wv.png", 
       plot = p, dpi = 100, width = 7, height = 8)

ll <- density_last_year("ROBO DE VEHICULO AUTOMOTOR S.V.")
p <- density_chart("car robbery wo/v", ll)
ggsave("graphs/density_car_robbery_wov.png", 
       plot = p, dpi = 100, width = 7, height = 8)

ll <- density_last_year("ROBO A TRANSEUNTE C.V.")
p <- density_chart("street robbery", ll)
ggsave("graphs/density_street_robbery.png", 
       plot = p, dpi = 100, width = 7, height = 8)

ll <- density_last_year("LESIONES POR ARMA DE FUEGO")
p <- density_chart("firearm lesions", ll)
ggsave("graphs/density_firearm.png", 
       plot = p, dpi = 100, width = 7, height = 8)


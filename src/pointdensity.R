

points <- read.csv("clean-data/crime-lat-long-pgj.csv", stringsAsFactors = FALSE) %>%
  filter(date >=  start_date & date < end_date) %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  na.omit()
SD_density <- pointdensity(df = points, lat_col = "lat", lon_col = "long",
                           date_col = "date", grid_size = 0.1, radius = 1)
qmplot(lon, lat, data = SD_density, geom = "blank",  
       zoom = 11, maptype = "terrain", darken = .4, legend = "topleft") +
  geom_point(aes(x = lon, y = lat, colour = count), shape = 19, size = 2,
             data = SD_density) + 
  scale_color_viridis(option = "inferno") +
  labs(title = "Point density of homicide counts in CDMX with a 1 km radius (Feb 2018-Jan 2019)") +
  coord_map()
ggsave("graphs/hommap.png", dpi = 100, width = 7, height = 8)




points <- read.csv("clean-data/crime-lat-long-pgj.csv", stringsAsFactors = FALSE) %>%
  filter(date >=  start_points & date < end_points) %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  na.omit()
SD_density <- pointdensity(df = points, lat_col = "lat", lon_col = "long",
                           date_col = "date", grid_size = 0.1, radius = 1)
SD_density$date <- as_date(SD_density$dateavg, origin = lubridate::origin)
breaks = c(as.numeric(as.Date(c("2017-02-01", "2018-02-01", "2019-02-01"))))
qmplot(lon, lat, data = SD_density, geom = "blank",  
       zoom = 11, maptype = "terrain", darken = .5, legend = "topleft") +
  geom_point(aes(x = lon, y = lat, colour = dateavg), shape = 19, size = 2,
             data = SD_density) +
  scale_color_distiller("average\ndate",
                        palette = "RdBu",
                        breaks = breaks,
                        labels = c(format(as.Date(breaks), "%b %Y"))) +
  labs(title = paste("Density count of homicides and average date of each point within a radius of 1km\n (",
                     format(as.Date(start_points), "%b %Y") ," - ",
                     format(as.Date(end_points) %m-% months(1), "%b %Y") , ")")) +
  coord_map()
ggsave("graphs/hommapdate.png", dpi = 100, width = 7, height = 8)

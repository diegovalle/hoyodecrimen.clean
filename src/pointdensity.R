
# Point density of homicides during the last year

points <- read.csv("clean-data/crime-lat-long-pgj.csv") %>%
  filter(date <= max(date) & 
           date > ceiling_date(
             floor_date(ymd(max(date)), "month") - years(1), 
             "month")
             ) %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  na.omit()
SD_density <- pointdensity(df = points, lat_col = "lat", lon_col = "long",
                           date_col = "date", grid_size = 0.1, radius = 1)


density_title <- str_c("Point density of homicide counts in CDMX with a 1 km",
                       " radius (",
                       format(as.Date(min(points$date)), "%b %Y"),
                       " - ",
                       format(as.Date(max(points$date)), "%b %Y"),
                       ")")
qmplot(lon, lat, data = SD_density, geom = "blank",
       zoom = 11, maptype = "terrain", darken = .4, legend = "topleft") +
  geom_point(aes(x = lon, y = lat, colour = count), shape = 19, size = 2,
             data = SD_density) +
  scale_color_viridis(option = "inferno") +
  labs(title = density_title) 
ggsave("graphs/hommap.png", dpi = 100, width = 7, height = 8)


# Density of changes in homicide rates (where have homicides decreased and 
# increased)

points <- read.csv("clean-data/crime-lat-long-pgj.csv") %>%
  filter(date <= max(date) & 
           date > ceiling_date(
             floor_date(ymd(max(date)), "month") - years(3), 
             "month")
  ) %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  na.omit()
SD_density <- pointdensity(df = points, lat_col = "lat", lon_col = "long",
                           date_col = "date", grid_size = 0.1, radius = 1)
SD_density$date <- as_date(SD_density$dateavg, origin = lubridate::origin)

int <- interval(min(SD_density$date), max(SD_density$date), 
                tz = "America/Mexico_City")
breaks  <-  c(
  as.numeric(
    as.Date(
      c(int$start + as.duration(int) / 3,
        int$start + 2 * as.duration(int) / 3,
        int$start + as.duration(int) - days(1)
      )
    )
  )
)


qmplot(lon, lat, data = SD_density, geom = "blank",
       zoom = 11, maptype = "terrain", darken = .5, legend = "topleft") +
  geom_point(aes(x = lon, y = lat, colour = dateavg), shape = 19, size = 2,
             data = SD_density) +
  scale_color_distiller("average\ndate",
                        palette = "RdBu",
                        breaks = breaks,
                        labels = c(format(as.Date(breaks), "%b %Y"))) +
  labs(title = paste0("Average date of point density counts of homicides",
                     " with a radius of 1km\n(",
                     format(as.Date(min(points$date)) + days(1), "%b %Y"),
                     " - ",
                     format(as.Date(max(points$date)), "%b %Y"),
                     ")")) 
ggsave("graphs/hommapdate.png", dpi = 100, width = 7, height = 8)

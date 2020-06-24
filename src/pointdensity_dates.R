print("point density and dates")
# Density of changes in homicide rates (i.e. where have homicides decreased and 
# increased)

density_points_date <- function(crime) {
  points <- read.csv("clean-data/crime-lat-long-pgj.csv") %>%
    filter(date <= max(date, na.rm = TRUE) & 
             date > ceiling_date(
               floor_date(ymd(max(date, na.rm = TRUE)), "month") - years(2), 
               "month")
    ) %>%
    filter(crime == !!crime) %>%
    na.omit()
  SD_density <- pointdensity(df = points, lat_col = "lat", lon_col = "long",
                             date_col = "date", grid_size = 0.5, radius = 1)
  SD_density$date <- as_date(SD_density$dateavg, origin = lubridate::origin)
  return(list("density" = SD_density, "start" = min(points$date),
              "end" = max(points$date, na.rm = TRUE)))
}

get_breaks <- function(ll) {
  int <- interval(min(ll[["density"]]$date), max(ll[["density"]]$date, na.rm = TRUE), 
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
  return(breaks)
}

plot_density_dates <- function(crime, ll) {
  ll$density <- subset(ll$density, lat != 0)
  ll$density <- subset(ll$density, lon != 0)
  breaks <- get_breaks(ll)
  qmplot(lon, lat, data =  ll[["density"]], geom = "blank",
         zoom = 11, maptype = "terrain", darken = .5, legend = "topleft") +
    geom_point(aes(x = lon, y = lat, colour = dateavg), shape = 19, size = 1,
               data =  ll[["density"]]) +
    scale_color_distiller("average\ndate",
                          palette = "RdBu",
                          breaks = breaks,
                          labels = c(format(as.Date(breaks), "%b %Y"))) +
    labs(title = paste0("Average date of point density counts of ",
                        crime,
                        " with a radius of 1km\n(",
                        format(as.Date(ll[["start"]]) + days(1), "%b %Y"),
                        " - ",
                        format(as.Date(ll[["end"]]), "%b %Y"),
                        ")")) 
}

ll <- density_points_date("HOMICIDIO DOLOSO")
p <- plot_density_dates("homicide", ll)
ggsave("graphs/dates_density_homicides.png", 
       plot = p, dpi = 100, width = 7, height = 8)

ll <- density_points_date("ROBO DE VEHICULO AUTOMOTOR C.V.")
p <- plot_density_dates("car robbery w/v", ll)
ggsave("graphs/dates_density_car_robbery_wv.png", 
       plot = p, dpi = 100, width = 7, height = 8)

ll <- density_points_date("ROBO DE VEHICULO AUTOMOTOR S.V.")
p <- plot_density_dates("car robbery wo/v", ll)
ggsave("graphs/dates_density_car_robbery_wov.png", 
       plot = p, dpi = 100, width = 7, height = 8)

ll <- density_points_date("ROBO A TRANSEUNTE C.V.")
p <- plot_density_dates("street robbery", ll)
ggsave("graphs/dates_density_street_robbery.png", 
       plot = p, dpi = 100, width = 7, height = 8)

ll <- density_points_date("LESIONES POR ARMA DE FUEGO")
p <- plot_density_dates("firearm lesions", ll)
ggsave("graphs/dates_density_firearm.png", 
       plot = p, dpi = 100, width = 7, height = 8)

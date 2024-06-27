print("GAM model of homicide rates in CDMX")


read_cdmx_map <- function() {
  if (cuadrantes_date == 2016) {
    suppressWarnings(
      cuad_map <- readOGR(file.path("shps_2016", "cuadrantes_population.shp"),
                          layer = "cuadrantes_population",
                          stringsAsFactors = FALSE,
                          encoding = "latin1",
                          use_iconv = TRUE,
                          verbose = FALSE)
    )
  } else {
    suppressWarnings(
      cuad_map <- readOGR(file.path("shps_2023", "cuadrantes_population_2023.shp"),
                          layer = "cuadrantes_population_2023",
                          stringsAsFactors = FALSE,
                          encoding = "latin1",
                          use_iconv = TRUE,
                          verbose = FALSE)
    )
    cuad_map@data$Sector2 <- cuad_map@data$Sector
    cuad_map@data$Sector_hoy <- cuad_map@data$Sector
  }
  cuad_map@data$Sector2 <- cuad_map@data$Sector
  cuad_map@data$Sector_hoy <- cuad_map@data$Sector
  cuad_map@data[which(cuad_map@data[, "Sector_hoy"] == "TAXQUEA"),
                "Sector_hoy"] <- "TAXQUEÑA"
  cuad_map@data[which(cuad_map@data[, "Sector"] == "TAXQUEA"),
                "Sector"] <- "TAXQUEÑA"
  cuad_map@data[which(cuad_map@data[, "Sector2"] == "TAXQUEA"),
                "Sector2"] <- "TAXQUEÑA"
  cuad_map@data$id <- 1:nrow(cuad_map@data)
  return(cuad_map)
}

gam_model <- function(df, nb, k, crime) {
  ctrl <- gam.control(nthreads = detectCores())
  
  # Use only one thread when not running on CI
  df$Sector_hoy <- as.factor(df$Sector_hoy)
  if (Sys.getenv("CI") == "") {
    m1 <- gam(count ~ s(id,
                        bs = "mrf",
                        k = k,
                        xt = list(nb = nb)) + s(Sector_hoy, bs = "re") + offset(log(SUMPOB1)),
              data = df,
              method = "REML",
              family = ziP
    )
  } else
    m1 <- gam(count ~ s(id,
                        bs = "mrf",
                        k = k,
                        xt = list(nb = nb)) + s(Sector_hoy, bs = "re") + offset(log(SUMPOB1)),
              data = df,
              method = "REML",
              family = ziP,
              control = ctrl
    )
  # m2 <- gam(count ~ s(id, 
  #bs = 'mrf', 
  #k = 300, 
  #xt = list(nb = nb)) + offset(log(SUMPOB1)) + Sector_hoy,
  #           data = df,
  #           method = 'REML', 
  #           family = tw
  # ) 
  #summary(m1)
  #summary(m2)
  #anova(m1, m2)
  #plot(m1, select=3)
  save(m1, file = paste0("clean-data/m1", crime, ".RData"))
  #load("clean-data/m1.RData")
  
  # df$resid.gam.mod <- residuals(m1, type = "pearson")
  # df$fit.gam.mod <- residuals(m1, type = "pearson")
  # plot(df$fit.gam.mod, df$resid.gam.mod)
  # ggplot(data = df) + geom_point(aes(x = count, y = resid.gam.mod)) + 
  #   facet_wrap(~Sector_hoy)
  # ggplot(data = df) + geom_line(aes(x = count, y = resid.gam.mod, group = Sector_hoy)) 
  return(m1)
}


get_dates <- function() {
  df <- read.csv("clean-data/crime-lat-long-pgj.csv") 
  
  # be careful about dates like 2016-02-29 which return NULL
  # when a year is substracted from them
  end_date <- max(ymd(df$date), na.rm = TRUE)
  start_date <- floor_date(end_date, "month") - years(1)
  start_date <- ceiling_date(start_date, "month")
  return(list("hom" = df, "start" = start_date, "end" = end_date))
}

# return a data.frame with the smoothed rates of 'crime'
gam_crime_last_year <- function(crime = "HOMICIDIO DOLOSO", cuad_map, k,
                                title = ""){
  ll <- get_dates()
  hom_last_year <- ll[["hom"]]
  end_date <- ll[["end"]]
  start_date <- ll[["start"]]
  
  hom_last_year <- hom_last_year %>%
    filter(date <= end_date & date >= start_date) %>%
    filter(crime == !!crime) %>%
    group_by(cuadrante) %>%
    summarise(count = n()) %>%
    filter(cuadrante != "(NO ESPECIFICADO)") %>%
    na.omit()
  
  stopifnot(nrow(hom_last_year) > 0)
  
  # get the neighbors of each cuadrante
  df <- droplevels(as(cuad_map, "data.frame"))
  nb <- poly2nb(as_Spatial(st_make_valid(st_as_sf(cuad_map))), row.names = df$id)
  names(nb) <- attr(nb, "region.id")
  # Manually add some neighbors to Polygon 708 (Topilejo - S-4.5.8)
  # besides polygon 287 (S-4.5.7):
  # 497 (S-4.5.5), 193 (S-4.5.6)
  nb[['708']] <- append(nb[['708']], as.integer(497))
  nb[['708']] <- append(nb[['708']], as.integer(193))
  nb[['497']] <- append(nb[['497']], as.integer(708))
  nb[['193']] <- append(nb[['193']], as.integer(708))
  
  df <- data.frame(df, hom_last_year[match(df$Nomenclatu,
                                           hom_last_year$cuadrante), ])
  df$count[is.na(df$count)] <- 0
  df$count[!is.finite(df$count)] <- 0
  df$Nomenclatu <- factor(df$Nomenclatu)
  df$SUMPOB1 <- as.integer(df$SUMPOB1)
  # No cuadrantes with zero population (besides, it doesn't happen IRL)
  df$SUMPOB1[df$SUMPOB1 < 100 ] <- 100
  df$SUMPOB1[is.na(df$SUMPOB1)] <- 100
  
  # Zero inflated?
  #ggplot(df, aes(count)) +
  #  geom_histogram(bins = 40)
  
  df$id <- as.factor(df$id)
  
  m1 <- gam_model(df, nb, k = k, crime)
  
  df.new <- df
  # pop column is equal to 1, so as to have log(populaton)=0,
  # but we need rates per 100K
  df.new$SUMPOB1 <- 100000
  df$pred <-  predict(m1, newdata = df.new, type = "response")
  # rate from the model
  df$pred_rate  <-  df$pred # / df$SUMPOB1 * 10^5
  # raw rate
  df$rate <- df$count / df$SUMPOB1 * 10 ^ 5
  return(list("hom" = df, "start" = start_date, "end" = end_date))
}


crime_gam_chart <- function(cuad_map, df, start_date, end_date) {
  labels <- data.frame(
    name = c("Tepito", " San Felipe de Jesús",
             "Cerro del Chiquihuite", "El Hoyo",
             "Central de Abastos", "Topilejo", "La Raza",
             "Zapotitlán", "San Miguel Ajusco"),
    lat = c(19.445793, 19.496768,
            19.542162, 19.3723,
            19.373099, 19.199720, 19.464604, 19.296392, 19.2218),
    long = c(-99.128877, -99.075112,
             -99.134397, -99.028994,
             -99.091441, -99.143017, -99.14233, -99.034282, -99.20707),
    group = NA
  )
  
 
  fcuadrantes <- fortify(cuad_map, region = "Nomenclatu")
  fcuadrantes$id <- as.factor(fcuadrantes$id)
  mdata <- left_join(fcuadrantes,
                     df[, c("Nomenclatu", "pred", "count", "pred_rate", "rate")],
                     by = c("id" = "Nomenclatu"))
  p <- ggplot(mdata, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = pred_rate)) +
    geom_path(col = "black", alpha = 0.5, size = 0.05) +
    coord_map() +
    geom_label_repel(data = labels, aes(long, lat, label = name), size = 3,
                     force = .8, alpha = .8,
                     box.padding = 3.3, label.padding = 0.18) +
    scale_fill_viridis(name = "rate",
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(75, units = "mm"),
                                              title.position = "left",
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
    theme_minimal() +
    theme(legend.position = "top",
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank()) +
    ggtitle(str_c("Modeled Homicide Rates in Mexico City (",
                  format(as.Date(start_date),
                         "%b %Y"),
                  " - ",
                  format(as.Date(end_date), "%b %Y"), ")"),
            subtitle = paste0("Because some cuadrantes have a low population",
                              " and homicides tend to be rare occurrences\n",
                              "the variance in crime rates per 100,000 tends",
                              " to be high. To remove some of the variance,\n",
                              "and help discover patterns in the data, the",
                              " crime rate in each cuadrante was calculated\n",
                              "based on a hierarchical GAM with a Gaussian Markov random",
                              " field smoother and a zero-inflated Poisson",
                              " \nresponse,",
                              "with each sector included as a treatment variable"))
  
  return(p)
}

k <- 710
cuad_map <- read_cdmx_map()

ll <- gam_crime_last_year("HOMICIDIO DOLOSO", cuad_map, k = k)
# `fortify(<SpatialPolygonsDataFrame>, region = ...)` is defunct' was
#p <- crime_gam_chart(cuad_map, ll[["hom"]], ll[["start"]], ll[["end"]])
#ggsave("graphs/cdmx-smooth-latest-HOMICIDIO.png",
#       plot = p, dpi = 100, width = 10, height = 13)

write(list(ll[["hom"]] %>%
             select( c("Nomenclatu", "rate", "pred_rate")) %>%
             mutate(rate = round(rate, 1),
                    pred_rate = round(pred_rate, 1)) %>%
             arrange(Nomenclatu) , 
           ll["start"], ll["end"]) %>%
        toJSON(dataframe = c("columns")),
      "clean-data/json/smooth-map-hom.json")

# ll <- gam_crime_last_year("ROBO DE VEHICULO AUTOMOTOR C.V.", cuad_map, k = 500)
# write(list(ll[["hom"]] %>%
#              select( c("Nomenclatu", "rate", "pred_rate")) %>%
#              mutate(rate = round(rate, 1),
#                     pred_rate = round(pred_rate, 1)) %>%
#              arrange(Nomenclatu) , 
#            ll["start"], ll["end"]) %>%
#         toJSON(dataframe = c("columns")),
#       "clean-data/json/smooth-map-rvcv.json")
# p <- crime_gam_chart(cuad_map, ll[["hom"]], ll[["start"]], ll[["end"]])
# ggsave("graphs/cdmx-smooth-latest.png-ROBO-DE-VEHICULO-CV.png", 
#        plot = p, dpi = 100, width = 10, height = 13)
# 
# ll <- gam_crime_last_year("ROBO DE VEHICULO AUTOMOTOR S.V.", cuad_map, k = k)
# write(list(ll[["hom"]] %>%
#              select( c("Nomenclatu", "rate", "pred_rate")) %>%
#              mutate(rate = round(rate, 1),
#                     pred_rate = round(pred_rate, 1)) %>%
#              arrange(Nomenclatu) , 
#            ll["start"], ll["end"]) %>%
#         toJSON(dataframe = c("columns")),
#       "clean-data/json/smooth-map-rvsv.json")
# p <- crime_gam_chart(cuad_map, ll[["hom"]], ll[["start"]], ll[["end"]])
# ggsave("graphs/cdmx-smooth-latest.png-ROBO-DE-VEHICULO-SV.png", 
#        plot = p, dpi = 100, width = 10, height = 13)
# 
# ll <- gam_crime_last_year("ROBO A TRANSEUNTE C.V.", cuad_map, k = k)
# write(list(ll[["hom"]] %>%
#              select( c("Nomenclatu", "rate", "pred_rate")) %>%
#              mutate(rate = round(rate, 1),
#                     pred_rate = round(pred_rate, 1)) %>%
#              arrange(Nomenclatu) , 
#            ll["start"], ll["end"]) %>%
#         toJSON(dataframe = c("columns")),
#       "clean-data/json/smooth-map-tran.json")
# p <- crime_gam_chart(cuad_map, ll[["hom"]], ll[["start"]], ll[["end"]])
# ggsave("graphs/cdmx-smooth-latest.png-ROBO-A-TRANSEUNTE-CV.png", 
#        plot = p, dpi = 100, width = 10, height = 13)
# 
ll <- gam_crime_last_year("LESIONES POR ARMA DE FUEGO", cuad_map, k = k)
write(list(ll[["hom"]] %>%
             select( c("Nomenclatu", "rate", "pred_rate")) %>%
             mutate(rate = round(rate, 1),
                    pred_rate = round(pred_rate, 1)) %>%
             arrange(Nomenclatu) , 
           ll["start"], ll["end"]) %>%
        toJSON(dataframe = c("columns")),
      "clean-data/json/smooth-map-laf.json")
# p <- crime_gam_chart(cuad_map, ll[["hom"]], ll[["start"]], ll[["end"]])
# ggsave("graphs/cdmx-smooth-latest.png-LESIONES-POR-ARMA-DE-FUEGO.png", 
#        plot = p, dpi = 100, width = 10, height = 13)

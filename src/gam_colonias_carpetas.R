print("GAM smooth model of homicide victimization rates in the colonias of CDMX")

get_dates <- function() {
  df <- read.csv("clean-data/crime-lat-long-pgj_carpetas.csv") 
  
  # be careful about dates like 2016-02-29 which return NULL
  # when a year is substracted from them
  end_date <- max(ymd(df$date), na.rm = TRUE)
  start_date <- floor_date(end_date, "month") - years(1)
  start_date <- ceiling_date(start_date, "month")
  return(list("hom" = df, "start" = start_date, "end" = end_date))
}

col <-st_make_valid( st_read("colonias/colonias_iecm.gpkg") )
dates <- get_dates()

# Add a sector column to the colonias shapefile
col_sec <- read.csv("shps_2023/colonias-sectores.csv")
col_sec <- select(col_sec, CVEUT, sector)
col <- left_join(col, col_sec, by = "CVEUT")

crimes <- filter(dates$hom, crime == "HOMICIDIO DOLOSO", !is.na(lat), !is.na(long),
                 date <= dates$end, date >= dates$start) |>
  st_as_sf(coords=c("long","lat"), crs=4326)
col$hom_count <- lengths(st_intersects(col, crimes))

# Set the polygons with errors to zero
# 02-083 SAN PABLO 396-CONJ HAB SAN PABLO (U HAB)
# 02-012 CRUZ ROJA TEPANTONGO (U HAB)
col$hom_count[which(col$CVEUT %in% c("02-012", "02-083"))] <- 0


centr <- st_as_sf(col) |> mutate(cntr = st_centroid(geom),
                                 within_dist = st_is_within_distance(cntr, dist = 750))
nb_distance <- centr[["within_dist"]]

nb_neighbor <- poly2nb(as_Spatial(st_as_sf(col)), row.names = col$ID)
# For some reason polygon id 1125 is missing 
names(nb_distance) <- attr(nb_neighbor, "region.id")
names(nb_neighbor) <- attr(nb_neighbor, "region.id")
nb_combined <- list()
for(i in names(nb_neighbor)) {
  nb_combined[[i]] <- unique(c(nb_neighbor[[i]], nb_distance[[i]]))
}
nb <- nb_neighbor

# Manually add some neighbors to Polygon '1276' - '12-115'
nb[[1188]] <- append(nb[[1188]], as.integer(1276))
nb[[1276]] <- append(nb[[1276]], as.integer(1188)) # island




nb[[1305]] <- append(nb[[1305]], as.integer(1276))
nb[[1276]] <- append(nb[[1276]], as.integer(1305))

nb[[1344]] <- append(nb[[1344]], as.integer(1276))
nb[[1276]] <- append(nb[[1276]], as.integer(1344))


nb[[238]] <- append(nb[[238]], as.integer(1276))
nb[[1276]] <- append(nb[[1276]], as.integer(238))


nb[[1262]] <- append(nb[[1262]], as.integer(1276))
nb[[1276]] <- append(nb[[1276]], as.integer(1262))

nb[[1343]] <- append(nb[[1343]], as.integer(1276))
nb[[1276]] <- append(nb[[1276]], as.integer(1343))

nb[[1530]] <- append(nb[[1530]], as.integer(1276))
nb[[1276]] <- append(nb[[1276]], as.integer(1530))


nb[[1284]] <- append(nb[[1284]], as.integer(1276))
nb[[1276]] <- append(nb[[1276]], as.integer(1284))

nb[[1343]] <- append(nb[[1343]], as.integer(1530))
nb[[1530]] <- append(nb[[1530]], as.integer(1343))

nb[[1344]] <- append(nb[[1344]], as.integer(1530))
nb[[1530]] <- append(nb[[1530]], as.integer(1344))

nb[[1188]] <- append(nb[[1188]], as.integer(1530))
nb[[1530]] <- append(nb[[1530]], as.integer(1188))

nb[[1323]] <- append(nb[[1323]], as.integer(1302))
nb[[1302]] <- append(nb[[1302]], as.integer(1323))


nb[[1171]] <- append(nb[[1171]], as.integer(1173))
nb[[1173]] <- append(nb[[1173]], as.integer(1171)) # island

nb[[1175]] <- append(nb[[1175]], as.integer(1171))
nb[[1171]] <- append(nb[[1171]], as.integer(1175)) # island

nb[[1165]] <- append(nb[[1165]], as.integer(1171))
nb[[1171]] <- append(nb[[1171]], as.integer(1165)) # 

nb[[1169]] <- append(nb[[1169]], as.integer(1171))
nb[[1171]] <- append(nb[[1171]], as.integer(1169)) # 

nb[[1170]] <- append(nb[[1170]], as.integer(1171))
nb[[1171]] <- append(nb[[1171]], as.integer(1170)) #

nb[[855]] <- append(nb[[855]], as.integer(854))
nb[[854]] <- append(nb[[854]], as.integer(855))

nb[[855]] <- append(nb[[855]], as.integer(954))
nb[[954]] <- append(nb[[954]], as.integer(855))

nb[[855]] <- append(nb[[855]], as.integer(856))
nb[[856]] <- append(nb[[856]], as.integer(855))

nb[[855]] <- append(nb[[855]], as.integer(831))
nb[[831]] <- append(nb[[831]], as.integer(855))

nb[[855]] <- append(nb[[855]], as.integer(789))
nb[[789]] <- append(nb[[789]], as.integer(855))


nb[[1252]] <- append(nb[[1252]], as.integer(1188))
nb[[1188]] <- append(nb[[1188]], as.integer(1252))

nb[[1252]] <- append(nb[[1252]], as.integer(1236))
nb[[1236]] <- append(nb[[1236]], as.integer(1252))



nb[[1187]] <- append(nb[[1187]], as.integer(1335))
nb[[1335]] <- append(nb[[1335]], as.integer(1187))

nb[[1240]] <- append(nb[[1240]], as.integer(1335))
nb[[1335]] <- append(nb[[1335]], as.integer(1240))


nb[[1228]] <- append(nb[[1228]], as.integer(1307))
nb[[1307]] <- append(nb[[1307]], as.integer(1228)) # island

nb[[1228]] <- append(nb[[1228]], as.integer(1216))
nb[[1216]] <- append(nb[[1216]], as.integer(1228)) # island

nb[[1228]] <- append(nb[[1228]], as.integer(1208))
nb[[1208]] <- append(nb[[1208]], as.integer(1228)) # island

nb[[1228]] <- append(nb[[1228]], as.integer(1193))
nb[[1193]] <- append(nb[[1193]], as.integer(1228)) # island

nb[[1228]] <- append(nb[[1228]], as.integer(1177))
nb[[1177]] <- append(nb[[1177]], as.integer(1228)) # island

nb[[1228]] <- append(nb[[1228]], as.integer(1312))
nb[[1312]] <- append(nb[[1312]], as.integer(1228)) # island

col$SUMPOB1[col$SUMPOB1 < 100 ] <- 100

df_col <- as.data.frame(col)
df_col$id <- as.character(df_col$ID)

setdiff(names(nb), df_col$id)
df_col$NOMDT <- as.factor(df_col$NOMDT)

print("running GAM")
start.time <- Sys.time()
m1 <- gam(hom_count ~ s(as.factor(id) + sector,
                        bs = "mrf",
                        k = 1600,
                        xt = list(nb = nb)) + offset(log(SUMPOB1)), #+ s(NOMDT, bs = "re") ,
          data = df_col,
          control = gam.control(nthreads = use_cores, trace = TRUE,
                                maxit = 50),
          method = "GCV",
          family = ziP
)
end.time <- Sys.time()
time.taken <- end.time - start.time
print("###################################")
print(paste0("GAM took: ", time.taken, " hours"))
print("###################################")

df.new <- df_col
# pop column is equal to 1, so as to have log(population)=0,
# but we need rates per 100K
df.new$SUMPOB1 <- 100000
pred <- predict(m1, newdata = df.new, se.fit = TRUE)


df_col$pred <- pred$fit
df_col$se.fit <- pred$se.fit
# rate from the model
df_col$pred_rate <- df_col$pred # / df_col$SUMPOB1 * 10^5
# raw rate
df_col$rate <- df_col$hom_count / df_col$SUMPOB1 * 10 ^ 5
col$pred_rate <- df_col$pred_rate
col$se.fit <- df_col$se.fit
col$rate <- df_col$rate
col$pred_rate_exp <- exp(col$pred_rate)


p <- ggplot(col) +
  geom_sf(aes(fill =exp(pred_rate)), color = "#111111", linewidth = .06) +
  scale_fill_viridis(option="H") +
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
                format(as.Date(dates$start),
                       "%b %Y"),
                " - ",
                format(as.Date(dates$end), "%b %Y"), ")"),
          subtitle = paste0("Because some colonias have low population numbers,",
                            " and homicides tend to be rare occurrences\n",
                            "the variance in crime rates per 100,000 tends",
                            " to be high. To remove some of the variance,\n",
                            "and help discover patterns in the data, the",
                            " crime rate in each cuadrante was calculated\n",
                            "based on a hierarchical GAM with a Gaussian Markov random",
                            " field smoother and a zero-inflated Poisson",
                            " \nresponse,",
                            "with each sector included as a treatment variable"))
ggsave("graphs/cdmx-smooth-latest-HOMICIDIO_carpetas.png",
       plot = p, dpi = 100, width = 20, height = 23, bg = "white")


# ggplot(col, aes(low)) +
# geom_histogram(bins = 200)

write(list(as.data.frame(col) %>%
             select( c("CVEUT", "SUMPOB1", "hom_count", 
                       "pred_rate", "se.fit")) %>%
             mutate(se.fit = round(se.fit, 1),
                    pred_rate = round(pred_rate, 1)) %>%
             rename("population" = "SUMPOB1") %>%
             arrange(CVEUT) , 
           list("start" = dates$start), list("end" = dates$end)) %>%
        toJSON(dataframe = c("columns")),
      "clean-data/json/smooth-map-colonias-hom.json")

is_inside_bbox <- function(lat, lon, lat_min, lat_max, lon_min, lon_max) {
  # Check if lat and lon vectors have the same length
  if (length(lat) != length(lon)) {
    stop("lat and lon vectors must have the same length")
  }
  
  # Validate bounding box coordinates
  if (lat_min > lat_max) {
    stop("lat_min must be less than or equal to lat_max")
  }
  if (lon_min > lon_max) {
    stop("lon_min must be less than or equal to lon_max")
  }
  
  # Check if points are within the bounding box
  inside <- (lat >= lat_min) & (lat <= lat_max) & 
    (lon >= lon_min) & (lon <= lon_max)
  
  return(inside)
}

mexico_city_inside <- is_inside_bbox(
  lat = df$Latitud, 
  lon = df$Longitud,
  lat_min = 18.9, 
  lat_max = 19.6,
  lon_min = -99.5, # More western longitude (larger negative number)
  lon_max = -98.7  # More eastern longitude (smaller negative number)
)
if (!all(mexico_city_inside, na.rm = TRUE))
  stop()

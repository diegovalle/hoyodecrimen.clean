
hom_last_year <- read.csv("clean-data/crime-lat-long-pgj.csv") 

# be careful about dates like 2016-02-29 which return NULL
# when a year is substracted from them
start_date <- hom_last_year$date %>%
  max %>%
  ymd %>%
  floor_date("month") - years(1)
start_date <- start_date %>%
  ceiling_date("month")
end_date <- max(hom_last_year$date)

hom_last_year <- hom_last_year %>%
  filter(date <= end_date & date >= start_date) %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  group_by(cuadrante) %>%
  summarise(count = n()) %>%
  filter(cuadrante != "(NO ESPECIFICADO)") %>%
  na.omit()


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
cuad_map@data$id <- 1:nrow(cuad_map@data)

df <- droplevels(as(cuad_map, "data.frame"))
nb <- poly2nb(cuad_map, row.names = df$id)
names(nb) <- attr(nb, "region.id")

df <- data.frame(df, hom_last_year[match(df$Nomenclatu,
                                         hom_last_year$cuadrante), ])
df$count[is.na(df$count)] <- 0
df$count[!is.finite(df$count)] <- 0
df$Nomenclatu <- factor(df$Nomenclatu)
df$SUMPOB1 <- as.integer(df$SUMPOB1)
# No cuadrantes with zero population, (besides it doensn't happen IRL)
df$SUMPOB1[df$SUMPOB1 == 0 ] <- 100

# Zero inflated?
ggplot(df, aes(count)) +
  geom_histogram()

ctrl <- gam.control(nthreads = 4)
df$id <- as.factor(df$id)
m1 <- gam(count ~ s(id,
                    bs = "mrf",
                    k = 710,
                    xt = list(nb = nb)) + offset(log(SUMPOB1)) + Sector_hoy,
          data = df,
          method = "REML",
          family = ziP
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
save(m1, file = "clean-data/m1.RData")
#load("clean-data/m1.RData")

# df$resid.gam.mod <- residuals(m1, type = "pearson")
# df$fit.gam.mod <- residuals(m1, type = "pearson")
# plot(df$fit.gam.mod, df$resid.gam.mod)
# ggplot(data = df) + geom_point(aes(x = count, y = resid.gam.mod)) + 
#   facet_wrap(~Sector_hoy)
# ggplot(data = df) + geom_line(aes(x = count, y = resid.gam.mod, group = Sector_hoy)) 


df.new <- df
#  pop column identically equal to 1, so to have log(populaton)=0,
# but we need rates per 100K
df.new$SUMPOB1 <- 100000
df$pred <-  predict(m1, newdata = df.new, type = "response")
# rate from the model
df$pred_rate  <-  df$pred # / df$SUMPOB1 * 10^5
# raw rate
df$rate <- df$count / df$SUMPOB1 * 10 ^ 5

labels <- data.frame(
  name = c("Valle de San Lorenzo", "Tepito", " San Felipe de Jesús",
           "Cerro del Chiquihuite", "Bellavista", "Ermita Zaragoza",
           "Central de Abastos", "San Andrés Tomatlán", "Topilejo",
           "Santa María Aztahuacan"),
  lat = c(19.299767, 19.445793, 19.496768,
         19.542162, 19.397080, 19.367161,
         19.373099, 19.327635, 19.199720,
         19.347272),
  long = c(-99.078852, -99.128877, -99.075112,
           -99.134397, -99.193402, -98.999505,
           -99.091441, -99.103074, -99.143017,
           -99.021737),
  group = NA
)

 
fcuadrantes <- fortify(cuad_map, region = "Nomenclatu")
fcuadrantes$id <- as.factor(fcuadrantes$id)
mdata <- left_join(fcuadrantes,
                   df[, c("Nomenclatu", "pred", "count", "pred_rate", "rate")],
                   by = c("id" = "Nomenclatu"))
ggplot(mdata, aes(x = long, y = lat, group = group)) +
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
                           "the variance in homicide rates per 100,000 tends",
                           " to be high. To remove some of the variance,\n",
                           "and help discover patterns in the data, the",
                           " homicide rate in each cuadrante was calculated\n",
                           "based on a GAM with a Gaussian Markov random",
                           " field smoother and a zero-inflated Poisson",
                           " response,\n",
                           "with each sector included as a treatment variable"))
ggsave("graphs/cdmx-smooth-latest.png", dpi = 100, width = 10, height = 13)
library('mgcv')
library('viridis')
library("spdep")
library("ggrepel")


hom_last_year <- read.csv("clean-data/crime-lat-long-pgj.csv", stringsAsFactors = FALSE) %>%
  filter(date <= "2018-12-31" & date >= "2018-01-01") %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  group_by(cuadrante) %>%
  summarise(count = n()) %>%
  filter(cuadrante != "(NO ESPECIFICADO)") %>%
  na.omit()

cuad_map <- readOGR(file.path("shps_2016", "cuadrantes_population.shp"), 
                    layer = "cuadrantes_population",
                    stringsAsFactors=FALSE,
                    encoding = "latin1", use_iconv=TRUE)
cuad_map@data[which(cuad_map@data[ ,"Sector_hoy"] == "TAXQUEA"), "Sector_hoy"] <- "TAXQUEÑA"
cuad_map@data[which(cuad_map@data[ ,"Sector"] == "TAXQUEA"), "Sector"] <- "TAXQUEÑA"
cuad_map@data[which(cuad_map@data[ ,"Sector2"] == "TAXQUEA"), "Sector2"] <- "TAXQUEÑA"
cuad_map@data$id <- 1:nrow(cuad_map@data)

df <- droplevels(as(cuad_map, 'data.frame'))
nb <- poly2nb(cuad_map, row.names = df$id)
names(nb) <- attr(nb, "region.id")

df <- data.frame(df, hom_last_year[match(df$Nomenclatu, hom_last_year$cuadrante),])
df$count[is.na(df$count)] <- 0
df$count[!is.finite(df$count)] <- 0
df$Nomenclatu <- factor(df$Nomenclatu)
df$SUMPOB1 <- as.integer(df$SUMPOB1)
# No cuadrantes with zero population, (besides it doensn't happen IRL)
df$SUMPOB1[df$SUMPOB1 == 0 ] <- 100 


#Some cuadrantes contain hospitals
# df$hospital <- FALSE
# df$hospital[df$cuadrante %in% c( "N-4.4.4", "C-2.1.16", "N-2.2.1", "O-2.5.7", 
#                                  "O-2.2.4", "N-1.3.10", "P-1.5.7", "P-3.1.1")] <- TRUE
# df$hospital <- as.factor(df$hospital)
# 
# df$municipio <- as.factor(df$municipio)
# 
# cuad.nb <- knn2nb(knearneigh(coordinates(cuad_map), k = 16), row.names = df$id)
# names(cuad.nb) <- attr(cuad.nb, "region.id")
# 
# ll <- lapply(cuad.nb, function(x) x)
# names(ll) <- as.character(cuadrantes$id)
# 
# hom$smooth <- sapply(1:nrow(hom), function(x) {
#   w <-  c(hom$population[x], hom$population[cuad.nb[[x]]])
#   r <- c(hom$rate[x], hom$rate[cuad.nb[[x]]])
#   return(sum(w * r)/sum(w))
# })


# Zero inflated?
ggplot(df, aes(count)) +
  geom_histogram(bins = 35)

ctrl <- gam.control(nthreads = 4)
m1 <- gam(count ~ s(id, bs = 'mrf', k = 200, xt = list(nb = nb)) + offset(log(SUMPOB1)), # define MRF smooth
          data = df,
          method = 'REML', # fast version of REML smoothness selection,
          family = tw
) 
m2 <- gam(count ~ s(id, bs = 'mrf', k = 200, xt = list(nb = nb)) + offset(log(SUMPOB1)), # define MRF smooth
          data = df,
          method = 'REML', # fast version of REML smoothness selection,
          family = ziP
) 
summary(m1)
summary(m2)
anova(m1, m2)
plot(m1, select=3)
#m1 <- m2

df$resid.gam.mod <- residuals(m1, type = "pearson")
df$fit.gam.mod <- residuals(m1, type = "pearson")
plot(df$fit.gam.mod, df$resid.gam.mod)
ggplot(data = df) + geom_point(aes(x = count, y = resid.gam.mod)) + 
  facet_wrap(~municipio)
ggplot(data = df) + geom_line(aes(x = count, y = resid.gam.mod, group = municipio)) 


df$pred = predict(m1, type = 'response')
df$pred_rate = df$pred  / df$SUMPOB1 * 10^5
df$rate <- df$count / df$SUMPOB1 * 10^5
#df$pred_rate2 <- df$pred_rate
#df$pred_rate2[df$pred_rate > 120] <- 120 

labels <- data.frame(
  name = c("Iztapalapa/Tlahuac", "Tepito", " San Felipe de Jesús", 
           "Cerro del Chiquihuite", "Olivar del Conde", "Ermita Zaragoza",
           "Central de Abastos"),
  lat =c(19.301887, 19.445793, 19.490038, 
         19.542162, 19.374862, 19.367161,
         19.373099),
  long = c(-99.069528, -99.128877, -99.076248, 
           -99.134397, -99.217309, -98.999505, 
           -99.091441),
  group = NA
)

 
fcuadrantes <- fortify(cuad_map, region = "Nomenclatu")
mdata <- left_join(fcuadrantes, df[,c("Nomenclatu", "pred", "count", "pred_rate", "rate")], by = c('id' = 'Nomenclatu'))
ggplot(mdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = pred_rate)) +
  geom_path(col = 'black', alpha = 0.5, size = 0.05) +
  coord_map() +
  geom_label_repel(data = labels, aes(long, lat, label = name), size = 3,
                   force = .2, alpha = .8,
                   box.padding = 3.3, label.padding = 0.18) +
  scale_fill_viridis(name = "rate", 
                     #limits = c(0, 120),
                     guide = guide_colorbar(direction = "horizontal",
                                            barheight = unit(2, units = "mm"),
                                            barwidth = unit(75, units = "mm"),
                                            title.position = 'left',
                                            title.hjust = 0.5,
                                            label.hjust = 0.5)) +
  theme_minimal() + 
  theme(legend.position = 'top',
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        #panel.spacing=unit(0, "lines"),
        plot.background=element_blank())+
  ggtitle(str_c("Modeled Homicide Rates in Mexico City (Jan 2018 - Dec 2018)"),
          subtitle = str_c("Because some cuadrantes have a low population and homicides tend to be rare occurrences\n",
                           "the variance in homicide rates per 100,000 tends to be high. To remove some of the variance,\n",
                           "and help discover patterns in the data, the homicide rate in each cuadrante was calculated\n",
                           "based on a GAM with a Gaussian Markov random field smoother and a Tweedie \n",
                           "response."))
ggsave("cdmx-smooth-latest.png", dpi = 100, width = 10, height = 11)


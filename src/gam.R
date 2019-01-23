


library('mgcv')
library('viridis')
library(spdep)


hom_last_year <- read.csv("clean-data/crime-lat-long.csv", stringsAsFactors = FALSE) %>%
  filter(date <= "2016-08-31" & date >= "2015-09-01") %>%
  filter(crime == "HOMICIDIO DOLOSO") %>%
  group_by(cuadrante) %>%
  summarise(count = n()) %>%
  filter(cuadrante != "(NO ESPECIFICADO)")


#mun.map <- mun.map[mun.map$region %in% unique(df$id),]


#mun.map <- mun.map[!mun.map$CVE_ENT %in% c("20"),]


df <- droplevels(as(cuadrantes, 'data.frame'))
nb <- poly2nb(cuadrantes, row.names = df$id)
names(nb) <- attr(nb, "region.id")

df <- data.frame(df, hom_last_year[match(df$id, hom_last_year$cuadrante),])
df$count[is.na(df$count)] <- 0
df$id <- factor(df$id)
df$SUMPOB1 <- as.integer(df$SUMPOB1)
# No cuadrantes with zero population, (besides it doensn't happen IRL)
df$SUMPOB1[df$SUMPOB1 == 0 ] <- 100 


#Some cuadrantes contain hospitals
df$hospital <- FALSE
df$hospital[df$cuadrante %in% c( "N-4.4.4", "C-2.1.16", "N-2.2.1", "O-2.5.7", 
                                 "O-2.2.4", "N-1.3.10", "P-1.5.7", "P-3.1.1")] <- TRUE
df$hospital <- as.factor(df$hospital)

df$municipio <- as.factor(df$municipio)

cuad.nb <- knn2nb(knearneigh(coordinates(cuadrantes), k = 16), row.names = df$id)
names(cuad.nb) <- attr(cuad.nb, "region.id")

ll <- lapply(cuad.nb, function(x) x)
names(ll) <- as.character(cuadrantes$id)

hom$smooth <- sapply(1:nrow(hom), function(x) {
  w <-  c(hom$population[x], hom$population[cuad.nb[[x]]])
  r <- c(hom$rate[x], hom$rate[cuad.nb[[x]]])
  return(sum(w * r)/sum(w))
})


# Zero inflated?
ggplot(df, aes(count)) +
  geom_histogram(bins = 35)

ctrl <- gam.control(nthreads = 4)
m1 <- gam(count ~ s(id, bs = 'mrf', k = 750, xt = list(nb = nb)) + offset(log(SUMPOB1)) + sector_hoyodecrimen, # define MRF smooth
          data = df,
          method = 'REML', # fast version of REML smoothness selection,
          family = tw
) 
m2 <- gam(count ~ s(id, bs = 'mrf', k = 840, xt = list(nb = cuad.nb)) + offset(log(SUMPOB1)), # define MRF smooth
          data = df,
          method = 'REML', # fast version of REML smoothness selection,
          family = ziP
) 
summary(m1)
summary(m2)
anova(m1, m2)
plot(m1, select=3)

df$resid.gam.mod <- residuals(m1, type = "pearson")
df$fit.gam.mod <- residuals(m1, type = "pearson")
plot(df$fit.gam.mod, df$resid.gam.mod)
ggplot(data = df) + geom_point(aes(x = count, y = resid.gam.mod)) + 
  facet_wrap(~municipio)
ggplot(data = df) + geom_line(aes(x = count, y = resid.gam.mod, group = municipio)) 


df$pred = predict(m1, type = 'response')
df$pred_rate = df$pred  / df$SUMPOB1 * 10^5
df$rate <- df$count / df$SUMPOB1 * 10^5
df$pred_rate2 <- df$pred_rate
df$pred_rate2[df$pred_rate > 100] <- 100 


mdata <- left_join(fcuadrantes, df[,c("id", "pred", "count", "pred_rate2", "rate")], by = c('id' = 'id'))
ggplot(mdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = pred_rate2)) +
  geom_path(col = 'black', alpha = 0.5, size = 0.1) +
  coord_map() +
  scale_fill_viridis(name = "rate", option = 'plasma',
                     limits = c(0, 100),
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
        panel.border = element_blank())

stops <- readOGR(file.path("cuadrante-shps", "stops.shp"), 
                 layer = "stops")
num.stops <- stops@data[,c("id",  "sector",  "PNTCNT")]
rm(stops)
names(num.stops)[1] <- "cuadrante"
num.stops <- join(mcrime, num.stops)
num.stops <- ddply(num.stops, .(cuadrante, sector, crime), summarise,
                   count = sum(count),
                   population = sum(population),
                   rate = (count / population) * 10^5 * 12/15,
                   PNTCNT = PNTCNT[1])
num.stops$has.stop <- ifelse(num.stops$PNTCNT > 0, TRUE, FALSE)

num.stops$rate[!is.finite(num.stops$rate)] <- NA
num.stops$rate[num.stops$rate == 0] <- 0.001
ggplot(subset(na.omit(num.stops), crime == "Robo a negocio C/V"), 
       aes(factor(has.stop), rate)) +
  geom_boxplot(color = "red", notch = TRUE) + 
  geom_jitter() +
  scale_y_log10() +
  ggtitle("")



ggplot(subset(na.omit(num.stops), crime == "Robo de vehiculo automotor S/V"), 
       aes(PNTCNT, rate)) +
  geom_point() +
  geom_smooth(method = MASS::rlm)+
  scale_y_log10()

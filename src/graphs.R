## Visually explore the crime data


## the data for Apr 2014 is incomplete!
date.total <- ddply(mcrime, .(crime, date), summarise,
                       count = sum(count))
na.omit(unique(mcrime$crime))
ggplot(date.total, 
       aes(as.Date(date), count)) +
  geom_line() +
  facet_wrap(~crime, scales = "free")
ggsave(file.path("graphs", "incomplete.png"), dpi = 100, width = 11, height = 8)
#mcrime <- subset(mcrime, !date %in% as.yearmon(badMonths))

ggplot(subset(date.total, crime %in% c("ROBO EN MICROBUS C.V.",
                                       "ROBO EN MICROBUS S.V.",
                                       "ROBO EN TAXI",
                                       "ROBO EN METRO S.V.",
                                       "ROBO EN METRO C.V.")), 
       aes(as.Date(date), count)) +
  geom_point() +
  facet_wrap(~crime) +
  geom_smooth() +
  xlab("fecha") +
  ylab("número de robos") +
  ggtitle("Robos en el Distrito Federal reportados a la SSPDF")
ggsave(file.path("graphs", "incomplete.png"), dpi = 100, width = 6, height = 4)


ddply(mcrime, .(crime), summarise,
                    count = length(count))
ddply(mcrime, .(crime), summarise,
      count = sum(count))
661/25


## How many cuadrantes belong to an undefined Sector?
unique(mcrime$cuadrante[is.na(mcrime$sector)])
mcrime$cuadrante[is.na(mcrime$cuadrante)]
length(unique(mcrime$cuadrante)) -1 # minus 1 because there's a cuadrante '(en blanco)'

#t <- cuadrantes@data[,c("id",  "sector",  "SUMPOB1")]
#t[which(t$id == "S-3.5.15"),]
#mcrime[which(mcrime$cuadrante == "S-3.5.15"),]

crime.cuadrante <- ddply(subset(mcrime, date >= "2014-05-01" & date <= "2015-04-01"), 
                 .(crime, cuadrante), summarise,
                 total = sum(count),
                 population = population[1],
                 sector = sector[1],
                 id = cuadrante[1],
                 rate = (sum(count, na.rm = TRUE)/ sum(population, na.rm = TRUE)) * 10 ^ 5)
crime.cuadrante <- crime.cuadrante[order(-crime.cuadrante$rate),]

crime.sector <- ddply(crime.cuadrante, 
                      .(crime, sector), summarise,
                      total = sum(total),
                      id = sector[1],
                      pop = sum(population, na.rm = TRUE),
                      rate = (sum(total, na.rm = TRUE)/ sum(pop, na.rm = TRUE)) * 10 ^ 5)
crime.sector <- crime.sector[order(-crime.sector$rate),]

#names(crime.cuadrante) <- c("crime", "id", "total", "population", "sector", "rate")
## Homicidio doloso == 749 vs SSPDF == 899
## Robo a negocio C/V == 4,239 vs SSP == 4290
## Robo de vehiculo automotor C/V == 5223 vs SSPDF == 5211
## Robo de vehiculo automotor S/V == 12,056 vs SSPDF == 12014
# ddply(mcrime, .(crime, year(date)), summarise, sum(count))
# totals.cuad <- ddply(mcrime, .(crime, cuadrante), summarise, 
#       count = sum(count),
#       population = population[1])
# ddply(totals.cuad, .(crime), summarise,
#       count = sum(count),
#       population = sum(population, na.rm = TRUE),
#       rate = (sum(count, na.rm = TRUE)/ sum(population, na.rm = TRUE)) * 10 ^ 5)

cuadrantes.map <- plyr::join(fcuadrantes, 
                             subset(crime.cuadrante, 
                                    crime == "HOMICIDIO DOLOSO"))
cuadrantes.map$rate2 <- cuadrantes.map$rate
cuadrantes.map$rate2[log(cuadrantes.map$rate) > 6.5 ] <- exp(6.5)
ggplot(cuadrantes.map, aes(long, lat, group = group, fill = total), color = "gray") +
  geom_polygon() +
  coord_map()+
  ggtitle("HOMICIDIO DOLOSO") +
  scale_fill_continuous(low = "#fee6ce",
                        high = "#e6550d", space = "Lab", na.value = "grey50",
                        guide = "colourbar")
ggsave(file.path("graphs", "map-cuadrante-total.png"), dpi = 100, width = 7, height = 7)

#names(crime.sector)[2] <- "id"
sector.map <- plyr::join(fsector, subset(crime.sector, 
                                         crime == "HOMICIDIO DOLOSO"))

ggplot(sector.map, aes(long, lat, group = group, fill = rate), color = "gray") +
  geom_polygon() +
  coord_map() +
  ggtitle("HOMICIDIO DOLOSO") +
  scale_fill_continuous(low = "#fee6ce",
                        high = "#e6550d", space = "Lab", na.value = "grey50",
                        guide = "colourbar")
ggsave(file.path("graphs", "map-sector-rate.png"), dpi = 100, width = 7, height = 7)



## Total crimes by date and crime type
date.crime <- ddply(mcrime, .(crime, date), summarise,
                    count = sum(count))
ggplot(date.crime, 
       aes(as.Date(date), count)) +
  geom_line() +
  facet_wrap(~crime)
ggsave(file.path("graphs", "total-ts.png"), dpi = 100, width = 9, height = 6)

date.sectores <- ddply(mcrime, .(sector, crime, date), summarise,
                       count = sum(count), 
                       population = sum(population, na.rm = TRUE),
                       rate = (count / population) * 10^5 * 12)


plotSectorRates <- function(df, crime, sub) {
  df <- subset(na.omit(df), crime == sub)
  df$sector <- with(df, reorder(sector, -rate, mean))
  ggplot(df, 
         aes(as.Date(date), rate, group = sector)) +
    geom_line() +
    facet_wrap(~sector) +
    #geom_smooth(method="loess", se = FALSE)  +
    #scale_y_continuous(limits = c(0, 140), breaks = c(0, 100)) +
    theme(strip.text.x = element_text(size = 6)) +
    ggtitle(str_c(crime, " rates in the DF, by sector (", as.yearmon(min(df$date)), " - ",
                                                                     as.yearmon(max(df$date)), ")")) +
    xlab("date") +
    ylab("rate") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

plotSectorRates_es <- function(df, crime, sub) {
  df <- subset(na.omit(df), crime == sub)
  df$sector <- with(df, reorder(sector, -rate, mean))
  ggplot(df, 
         aes(as.Date(date), rate, group = sector)) +
    geom_line() +
    facet_wrap(~sector) +
    #geom_smooth(method="loess", se = FALSE)  +
    #scale_y_continuous(limits = c(0, 140), breaks = c(0, 100)) +
    theme(strip.text.x = element_text(size = 6)) +
    ggtitle(str_c("Tasas de ", crime, "(", as.yearmon(min(df$date)), " - ",
                  as.yearmon(max(df$date)), ")")) +
    xlab("fecha") +
    ylab("tasa") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

plotSectorRates(date.sectores, "Homicide", "Homicidio doloso")
ggsave(file.path("graphs", "total-sector-hom.png"), dpi = 100, width = 10, height = 7)

plotSectorRates(date.sectores, "Robbery to a business with violence", "Robo a negocio C.V.")
ggsave(file.path("graphs", "total-sector-rncv.png"), dpi = 100, width = 10, height = 7)

plotSectorRates(date.sectores, "Car robbery with violence", "Robo de vehiculo automotor C.V.")
ggsave(file.path("graphs", "total-sector-rvcv.png"), dpi = 100, width = 10, height = 7)

plotSectorRates(date.sectores, "Car robbery without violence", "Robo de vehiculo automotor S.V.")
ggsave(file.path("graphs", "total-sector-rvsv.png"), dpi = 100, width = 10, height = 7)

plotSectorRates(date.sectores, "Rape", "Violacion")
ggsave(file.path("graphs", "total-sector-viol.png"), dpi = 100, width = 10, height = 7)

plotSectorRates(date.sectores, "Robbery to a pedestrian with violence", "Robo a transeunte C.V.")
ggsave(file.path("graphs", "total-sector-rtcv.png"), dpi = 100, width = 10, height = 7)

plotSectorRates(date.sectores, "Robbery to a pedestrian without violence", "Robo a transeunte S.V.")
ggsave(file.path("graphs", "total-sector-rtsv.png"), dpi = 100, width = 10, height = 7)

plotSectorRates(date.sectores, "Home invasion", "Robo a casa habitacion C.V.")
ggsave(file.path("graphs", "total-sector-rccv.png"), dpi = 100, width = 10, height = 7)

# Espanol

plotSectorRates_es(date.sectores, "homicidio doloso", "Homicidio doloso")
ggsave(file.path("graphs", "total-sector-hom_es.png"), dpi = 100, width = 10, height = 7)

plotSectorRates_es(date.sectores, "robo a negocio C.V.", "Robo a negocio C.V.")
ggsave(file.path("graphs", "total-sector-rncv_es.png"), dpi = 100, width = 10, height = 7)

plotSectorRates_es(date.sectores, "robo de vehiculo automotor C.V.", "Robo de vehiculo automotor C.V.")
ggsave(file.path("graphs", "total-sector-rvcv_es.png"), dpi = 100, width = 10, height = 7)

plotSectorRates_es(date.sectores, "robo de vehiculo automotor S.V.", "Robo de vehiculo automotor S.V.")
ggsave(file.path("graphs", "total-sector-rvsv_es.png"), dpi = 100, width = 10, height = 7)

plotSectorRates_es(date.sectores, "violacion", "Violacion")
ggsave(file.path("graphs", "total-sector-viol_es.png"), dpi = 100, width = 10, height = 7)

plotSectorRates_es(date.sectores, "robo a transeunte C.V.", "Robo a transeunte C.V.")
ggsave(file.path("graphs", "total-sector-rtcv_es.png"), dpi = 100, width = 10, height = 7)

plotSectorRates_es(date.sectores, "robo a transeunte S.V.", "Robo a transeunte S.V.")
ggsave(file.path("graphs", "total-sector-rtsv_es.png"), dpi = 100, width = 10, height = 7)

plotSectorRates_es(date.sectores, "robo a casa habitacion C.V.", "Robo a casa habitacion C.V.")
ggsave(file.path("graphs", "total-sector-rccv_es.png"), dpi = 100, width = 10, height = 7)


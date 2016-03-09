topoNames <- c("id", "hom", "rncv", "rvcv", "rvsv", "viol")
topoRateNames <- c("id", "population", "hom_rate", "rncv_rate", "rvcv_rate", "rvsv_rate", "viol_rate")
topoCountNames <- c("id", "population", "hom_count", "rncv_count", "rvcv_count", "rvsv_count", "viol_count")

topoSectorNames <- c("sector", "hom", "rncv", "rvcv", "rvsv", "viol")
topoDiffNames <- c("id",  "hom_current", "hom_last", 
                   "rncv_current", "rncv_last", 
                   "rvcv_current", "rvcv_last", 
                   "rvsv_current", "rvsv_last", 
                   "viol_current", "viol_last")
interactiveRateNames <- c("sector", "population", "hom_rate", "rncv_rate", 
                          "rvcv_rate", "rvsv_rate", "viol_rate")
interactiveCountNames <- c("sector", "population", "hom_count", "rncv_count", 
                           "rvcv_count", "rvsv_count", "viol_count")
formatCuadranteForJSON <- function(mcrime, crime.type) {
  js <- ddply(subset(mcrime[order(mcrime$cuadrante, mcrime$date),], 
                     crime == crime.type)[,c("cuadrante", "count")],
              .(cuadrante), function(x) x$count)
  js <- subset(js, cuadrante != "(en blanco)")
  nam <- js$cuadrante
  js$cuadrante <- NULL
  js <- as.data.frame(t(js))
  names(js) <- nam
  return(js)
}

formatSectorForJSON <- function(mcrime, crime.type) {
#   js <- ddply(subset(mcrime[order(mcrime$cuadrante, mcrime$date),], 
#                crime == crime.type),
#         .(cuadrante, date), summarise, 
#         count = sum(count),
#         population = population[1])
  js <- ddply(subset(mcrime[order(mcrime$sector, mcrime$date),], 
                     crime == crime.type),
              .(sector, date), summarise, 
              count = (sum(count) / sum(population, na.rm = TRUE)) * 10^5 * 12,
              population = sum(population))
  
  js <- subset(js, sector != "(en blanco)")
  js <- ddply(js[,c("sector", "count")],
              .(sector), function(x) x$count)
  nam <- js$sector
  js$sector <- NULL
  js <- as.data.frame(t(js))
  names(js) <- nam
  return(js)
}


## For the initial line chart diaply with total data
date.total <- ddply(mcrime, .(crime, date), summarise,
                    count = sum(count))
date.total$date <- as.Date(date.total$date)
total <- cast(date.total, date ~ crime, value = "count")
names(total) <- c("date", "HomicidesA", "rncvA", "rvcvA", "rvsvA", "violA")
txt <- toJSON(total[,-1], dataframe = "column")
txt <- toJSON(total[,-1], dataframe = "column")
txt <- str_replace_all(txt, '"|\\{|\\}', "")
txt <- str_replace_all(txt, ':', "=")
txt <- str_replace_all(txt, ' $', ";")
txt <- str_replace_all(txt, 'HomicidesA = \\[', "HomicidesA = \\['Homicides',")
txt <- str_replace_all(txt, 'rncvA = \\[', "rncvA = \\['Violent robberies to a business',")
txt <- str_replace_all(txt, 'rvcvA = \\[', "rvcvA = \\['Violent car robberies',")
txt <- str_replace_all(txt, 'rvsvA = \\[', "rvsvA = \\['Non-violent car robberies',")
txt <- str_replace_all(txt, 'violA = \\[', "violA = \\['Rape',")
write(txt, file=file.path("interactive-maps","tables", "vecCuadrantes.txt"))


#for merging with cuadrante topojson
topo <- ddply(subset(mcrime, as.Date(as.yearmon(date)) >= as.Date(yearAgo)), 
              .(crime, cuadrante), summarise,
              count = sum(count))
topo <- cast(topo, cuadrante ~ crime, value = "count")
names(topo) <- topoNames
topo <- subset(topo, id != "(en blanco)")
apply(topo[,-1], 2, function(x) range(x))
write.csv(topo, "data/topo-cuadrantes.csv", row.names = FALSE)

#for merging with cuadrante topojson for the difference map
mcrime2 <- mcrime
mcrime2$when <- NA
mcrime2$when[mcrime2$date %in% as.yearmon(lastyearMonths)] <- "last"
mcrime2$when[mcrime2$date %in% as.yearmon(thisyearMonths)] <- "current"
mcrime2 <- subset(mcrime2, !is.na(mcrime2$when))
topo <- ddply(subset(mcrime2, date %in% as.yearmon(c(lastyearMonths, thisyearMonths))), 
              .(crime, cuadrante, when), summarise,
              count = sum(count))
topo <- cast(topo, cuadrante ~ crime + when, value = "count")
names(topo) <- topoDiffNames
topo$hom <- topo$hom_current - topo$hom_last
topo$rncv <- topo$rncv_current - topo$rncv_last
topo$rvcv <- topo$rvcv_current - topo$rvcv_last
topo$rvsv <- topo$rvsv_current - topo$rvsv_last
topo$viol <- topo$viol_current - topo$viol_last
topo <- subset(topo, id != "(en blanco)")


names(topo)[1] <-  "id"
cuadrantes.map <- plyr::join(fcuadrantes, topo )
ggplot(cuadrantes.map, aes(long, lat, group = group, fill = hom), color = "gray") +
  geom_polygon() +
  coord_map()+
  ggtitle("") +
  scale_fill_gradient2(low = "#4575b4",
                       mid = "#ffffbf",
                        high = "#d73027", space = "Lab", na.value = "grey50",
                        guide = "colourbar")


createTable <- function(topo, var, name) {
  topo$rank <- rank(-topo[[var]])
  topo <- merge(topo, unique(mcrime[,c("cuadrante", "sector", "population")]), 
                by.x = "id", by.y = "cuadrante",
                all.x = TRUE)
  topo <- topo[order(topo$rank),]
  topo$population <- prettyNum(topo$population, big.mark = ",")
  xtab <- xtable(prettyNum(subset(topo[,c("rank", "id", "sector", "population",  
                                          str_c(var, "_current"), 
                                          str_c(var, "_last"), 
                                          var)], 
                                  rank<=topo$rank[10])), 
                 digits = c(2,0, 0,0,0,0,0,0), 
                 caption = "Top quadrants with the highest increases in crimes")
  names(xtab) <- c("rank", "quadrant", "sector", "population", 
                   str_c(name, " May-Jul 2014"), 
                   str_c(name, " May-Jul 2013"),
                   "difference")
  print(xtab, type='html', include.rownames=FALSE,
        file=file.path('interactive-maps', 
                       'tables',
                       str_c("table-", var, 
                             "-diff", "-cuadrantes.html")))
}
createTable(topo, "hom", "homicides")
createTable(topo, "rncv", "robbery to a business with violence")
createTable(topo, "rvcv", "vehicle robbery with violence")
createTable(topo, "rvsv", "vehicle robbery without violence")
createTable(topo, "viol", "rape")

#names(topo) <- topoNames
topo <- topo[,c(1,12:16)]

#apply(topo[,-1], 2, function(x) range(x))
write.csv(topo, "data/topo-cuadrantes-diff.csv", row.names = FALSE)

#for merging with the leaflet topojson
topo <- ddply(subset(mcrime, date >= yearAgo),
              .(crime, cuadrante), summarise,
              count = sum(count),
              population = population[1],
              rate = round((sum(count, na.rm = TRUE) / 
                       sum(population, na.rm = TRUE)) * 10 ^ 5, 1))
topo.count <- cast(topo, cuadrante + population ~ crime, value = "count")
topo.rate <- cast(topo, cuadrante + population ~ crime, value = "rate")
names(topo.count) <- topoCountNames
names(topo.rate) <- topoRateNames
topo <- merge(topo.rate, topo.count)
topo <- subset(topo, id != "(en blanco)")
apply(topo[,-1], 2, function(x) range(x))
write.csv(topo, "data/interactive-cuadrantes.csv", row.names = FALSE)





js <- list(hom=formatCuadranteForJSON(mcrime, "Homicidio doloso"),
           rncv=formatCuadranteForJSON(mcrime, "Robo a negocio C/V"),
           rvcv=formatCuadranteForJSON(mcrime, "Robo de vehiculo automotor C/V"),
           rvsv=formatCuadranteForJSON(mcrime, "Robo de vehiculo automotor S/V"),
           viol=formatCuadranteForJSON(mcrime, "Violacion"))
js <- toJSON(js, dataframe = "column")
fh <- file("html/js/hom-dol-cuad.json", "w")
writeLines(js, fh)
close(fh)


## For the initial line chart diaply with total data
date.total <- ddply(mcrime, .(crime, date), summarise,
                    rate = (sum(count) / sum(population, na.rm = TRUE)) * 10^5 * 12)
date.total$date <- as.Date(date.total$date)
total <- cast(date.total, date ~ crime, value = "rate")
names(total) <- c("date", "HomicidesA", "rncvA", "rvcvA", "rvsvA", "violA")
txt <- toJSON(total[,-1], dataframe = "column")
txt <- str_replace_all(txt, '"|\\{|\\}', "")
txt <- str_replace_all(txt, ':', "=")
txt <- str_replace_all(txt, ' $', ";")
txt <- str_replace_all(txt, 'HomicidesA = \\[', "HomicidesA = \\['Homicide rate',")
txt <- str_replace_all(txt, 'rncvA = \\[', "rncvA = \\['Violent robbery to a business rate',")
txt <- str_replace_all(txt, 'rvcvA = \\[', "rvcvA = \\['Violent car robbery rate',")
txt <- str_replace_all(txt, 'rvsvA = \\[', "rvsvA = \\['Non-violent car robbery rate',")
txt <- str_replace_all(txt, 'violA = \\[', "violA = \\['Rape rate',")
write(txt, file=file.path("interactive-maps","tables", "vecSector.txt"))

##########33
#Sector data


js <- list(hom=formatSectorForJSON(mcrime, "Homicidio doloso"),
           rncv=formatSectorForJSON(mcrime, "Robo a negocio C/V"),
           rvcv=formatSectorForJSON(mcrime, "Robo de vehiculo automotor C/V"),
           rvsv=formatSectorForJSON(mcrime, "Robo de vehiculo automotor S/V"),
           viol=formatSectorForJSON(mcrime, "Violacion"))
js <- toJSON(js, dataframe = "column")
fh <- file("html/js/hom-dol-sector.json", "w")
writeLines(js, fh)
close(fh)



#for merging with sector topojson
crime.cuadrante <- ddply(subset(mcrime, date >= yearAgo),
                      .(crime, cuadrante), summarise,
                      total = sum(count),
                      population = population[1],
                      sector = sector[1])

crime.sector <- ddply(crime.cuadrante, 
                      .(crime, sector), summarise,
                      total = sum(total),
                      population = sum(population, na.rm = TRUE),
                      count = round((sum(total, na.rm = TRUE) / 
                                       sum(population, na.rm = TRUE)) * 10 ^ 5, 1))
sum(subset(crime.sector, crime == "Homicidio doloso")$population)
topo <- crime.sector[,c("sector", "crime", "count", "population", "total")]
topo$count <- round(topo$count, 1)
topo <- cast(topo, sector ~ crime, value = "count")
names(topo) <- topoSectorNames
topo <- subset(topo, sector != "(en blanco)")
apply(topo[,-1], 2, function(x) range(x))
write.csv(topo, "data/topo-sectores.csv", row.names = FALSE)

interactive.map <- crime.sector[,c("sector", "crime", "count", "population", "total")]
interactive.map$count <- round(interactive.map$count, 1)
interactive.map.rate <- cast(interactive.map, sector + population ~ crime, value = "count")
interactive.map.total <- cast(interactive.map, sector + population ~ crime, value = "total")
names(interactive.map.rate) <- interactiveRateNames
names(interactive.map.total) <- interactiveCountNames
interactive.map <- merge(interactive.map.total, interactive.map.rate)
interactive.map <- na.omit(interactive.map)
apply(interactive.map[,-1], 2, function(x) range(x))
write.csv(interactive.map, "data/interactive-sectores.csv", row.names = FALSE)


## Total for when the map is first displayed
tot <- ddply(subset(mcrime, date >= yearAgo),
             .(crime), summarise,
             count = sum(count),
             population = sum(population, na.rm = TRUE) / 12,
             rate = round((sum(count, na.rm = TRUE) / 
                             sum(population, na.rm = TRUE)) * 10 ^ 5, 1))


write(str_c('homTotal = ', tot$count[1], ',',
      'rncvTotal = ', tot$count[2], ',',
      'rvcvTotal = ', tot$count[3], ',',
      'rvsvTotal = ', tot$count[4], ',',
      'violTotal = ', tot$count[5], ',',
      'homTotalRate = ', tot$rate[1], ',',
      'rncvTotalRate = ', tot$rate[2], ',',
      'rvcvTotalRate = ', tot$rate[3], ',',
      'rvsvTotalRate = ', tot$rate[4], ',',
      'violTotalRate = ', tot$rate[5], ';'
), "interactive-maps/totals.js")
      
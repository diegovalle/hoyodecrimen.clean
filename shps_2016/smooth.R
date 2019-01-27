library(rgdal)
library(maptools)
require(spdep)
library(spweights)
library(jsonlite)
library(ggplot2)
library(DCluster)
library(jsonlite)

cuad <- readOGR("cuadrantes_population.shp", "cuadrantes_population")
plot(cuad)
names <- cuad@data$Nomenclatu
cuad.nb <- poly2nb(cuad, row.names = as.character(cuad$Nomenclatu))

cuad.nb <- knn2nb(knearneigh(coordinates(cuad), k = 8), row.names = as.character(cuad$Nomenclatu))
write.nb.gal(cuad.nb, "cuadrantes.gal")

class(cuad.nb) <- "list"

ll[[1]] <- cuad.nb
ll[[2]] <- attr(cuad.nb, "region.id")

write(toJSON(ll), "cuadrantes.neighbors.json")


class(cuad.nb) <- "nb"

#Cuadrantes with hospitals
# O-2.5.7
# O-2.2.4
# N-4.4.4*
# N-1.3.10
# C-2.1.16*
# N-2.2.1
# P-1.5.7
# P-3.1.1

hom <- fromJSON("https://hoyodecrimen.com/api/v1/cuadrantes/ALL/crimes/HOMICIDIO%20DOLOSO/period")$rows
hom <- subset(hom, cuadrante != "(NO ESPECIFICADO)")
# match the order with the neighborhood file
hom <- hom[match(cuad$Nomenclatu, hom$cuadrante),]
# fill in the population of cuadrantes with zero residents
# with the mean of their neighboring cuadrantes
for(zero_cuad in hom$cuadrante[which(hom$population == 0)])
  hom$population <- mean(hom$population[cuad.nb[[which(hom$cuadrante == zero_cuad)]]])


hom$rate <- hom$count / hom$population * 10^5
write.csv(hom, 'hom.csv')

which.max(hom$rate)
hom$rate[which.max(hom$rate)]

#smth<-empbaysmooth(hom$count, hom$population * sum(hom$count) / sum(hom$population))
#hom$smooth <- smth$smthrr


hom$smooth <- sapply(1:nrow(hom), function(x) {
  w <-  c(hom$population[x], hom$population[cuad.nb[[x]]])
  r <- c(hom$rate[x], hom$rate[cuad.nb[[x]]])
  return(sum(w * r)/sum(w))
})

which.max(hom$smooth)
hom$smooth[which.max(hom$smooth)]

cuad.f <- fortify(cuad, region = "Nomenclatu")
cuad.f <- merge(cuad.f, hom, by.x = "id", by.y = "cuadrante")

ggplot(cuad.f, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = smooth)) +
  coord_map()


# In spatial epidemiology, raw incidence rates can be statistically
# unstable due to the rarity of the disease and the relative small
# number of people at risk.

m <- readOGR("shps_2016/cuadrantes.shp", "cuadrantes")
plot(m)
crs <- proj4string(m)
region <- gUnaryUnion(m, id = m@data$Sector_hoy)
df <- data.frame(sector=unique(m@data$Sector_hoy))
row.names(df) <- df$sector
plot(region)
spp <- SpatialPolygonsDataFrame(region,
                                data = df)
proj4string(spp)
writeOGR(spp, "shps_2016/.", "sectores", driver="ESRI Shapefile",
         overwrite_layer = TRUE)

# 
# m <- readOGR("shps_2016/manzanas_points.shp", "manzanas_points")
# m@data <- m@data[ ,c(1:2, ncol(m@data))]
# writeOGR(m, "shps_2016/.", "manzanas_points", driver="ESRI Shapefile",
#          overwrite_layer = TRUE)

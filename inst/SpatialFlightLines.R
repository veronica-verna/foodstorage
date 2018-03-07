# This script contains three results:
## producersInfoStraight: a SpatialLinesDataFrame that contains all transport-routes as straight lines on the map
## producersInfo: a SpatialLinesDataFrame that contains all transport-routes as great-circle lines on the map
## producersRoutes: a SpatialLinesDataFrame that contains the transport-routes from the Kornkammer to its suppliers as actual driving routes on the streets.

## create line for every path to the Kornkammer:
linefileStr <- lapply(1:nrow(totalDistances), createLines)
producersStraight <- SpatialLines(linefileStr, proj4string = crs(producersExist)) #crs(producersExist)
producersInfoStraight <- SpatialLinesDataFrame(producersStraight, totalDistances)

writeOGR(producersInfoStraight, "data/producersInfoStraight/", "producersInfoStraight", "ESRI Shapefile")
#test <- readOGR("data/producersInfoStraight/", "producersInfoStraight")

rm(producersStraight)
rm(linefileStr)
###################
library("geosphere")

liste <- list()
for(i in 1:nrow(totalDistances)){
  liste[[i]] <- createCurves(i)
}

producersL <- SpatialLines(liste, proj4string = crs(producersExist)) #crs(producersExist)
producersInfo <- SpatialLinesDataFrame(producersL, totalDistances)

writeOGR(producersInfoStraight, "data/producersInfo/", "producersInfo", "ESRI Shapefile")

rm(liste)
rm(producersL)

################
library(osrm)
library(raster)
# Get route from OSM: package osrm
## transform the data to the needed format for the osrmRoute()-function 
KoKa <- data.frame(id = Kornkammer$Name, lon = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2])
prodEx <- data.frame(id = producersExist$Lieferant, lon = coordinates(producersExist)[,1], coordinates(producersExist)[,2])

lineRoutes <- lapply(1:nrow(prodEx), function(x) Lines(Line(osrmRoute(KoKa, prodEx[x,], sp = F)), ID = x))
row.names(prodEx) <- as.character(1:19)
lR <- SpatialLines(lineRoutes, proj4string= crs(producersExist))
producersRoutes <- SpatialLinesDataFrame(lR, prodEx)

writeOGR(producersRoutes, "data/producersRoutes/", "producersRoutes", "ESRI Shapefile")

rm(KoKa, lineRoutes, lR)

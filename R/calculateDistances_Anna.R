library(RSQLite)
library(readr)
library(data.table)
library(sf)
library(ggplot2)
library(tmap)
library(rgdal)
library(dplyr)
library(raster)

con <- dbConnect(SQLite(), "../foodstorage/data/kornInfo.sqlite")

dbListTables(con)
dbListFields(con, "productOrigin")
dbListFields(con, "producerAdress")


origin <- dbGetQuery(con, "SELECT * FROM productOrigin")
originExist <- origin[-which(is.na(origin$xCoord)),]

# the coordinates in the order of the existing origins
producerCoords <- left_join(originExist[, c("Lieferant", "Produkte_Zusammenfassung")], producers[, c("Lieferant", "xCoord", "yCoord")])

# convert originExist to spatialpointsdataframe
coordinates(originExist) <- ~xCoord + yCoord

#Kornkammer <- dbGetQuery(con, "SELECT * from AdresseKornkammer")
producers <- dbGetQuery(con, "SELECT * FROM producerAdress")
producersExist <- producers[-which(is.na(producers$xCoord)),]

#coordinates(Kornkammer) <- ~xCoord + yCoord
coordinates(producerCoords) <- ~xCoord + yCoord
#crs(Kornkammer) <-  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
crs(producerCoords) <-  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
crs(originExist) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

originExist$EntfernungZwischenhaendler <- spDists(coordinates(originExist), coordinates(producerCoords), longlat=T, diagonal = T)
Distances2 <- data.table(Lieferant = originExist$Lieferant, EntfernungZwischenhaendler = originExist$EntfernungZwischenhaendler)

origin <- as.data.table(left_join(origin, Distances2, by="Lieferant"))
# write Distance to KK into the database table producerAdress
#dbWriteTable(con, "producerAdress", producers)


library(RSQLite)
library(readr)
library(data.table)
library(sf)
library(ggplot2)
library(tmap)
library(rgdal)
library(dplyr)
library(raster)

con <- dbConnect(SQLite(), "data/kornInfo.sqlite")

#produzenten <- read.csv2("data/Produzenten.csv")
#dbWriteTable(con, "producerAdress", produzenten)

dbListTables(con)
dbListFields(con, "productInfo")
dbListFields(con, "producerAdress")


#kopf <- dbGetQuery(con, "SELECT * FROM productInfo LIMIT 10")
#kopf
#kopf[2,4]

producers <- dbGetQuery(con, "SELECT * FROM producerAdress")
producers$xCoord <- as.numeric(producers$xCoord)
producers$yCoord <- as.numeric(producers$yCoord)

producersExist <- producers[-which(is.na(producers$xCoord)),]

# convert producers to spatialpointsdataframe
coordinates(producersExist) <- ~xCoord + yCoord

dbDisconnect(con)

Kornkammer <- data.table(Strasse = "Höheweg 3", PLZ= "79104", Ort = "Freiburg")
Kornkammer[1, xCoord := geocode(paste0(Strasse, ", ", PLZ, " ", Ort))[1]]
Kornkammer[1, yCoord := geocode(paste0(Strasse, ", ", PLZ, " ", Ort))[2]]
coordinates(Kornkammer) <- ~xCoord + yCoord
crs(Kornkammer) <-  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

crs(producersExist) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

producersExist$EntfernungKK <- spDistsN1(producersExist, Kornkammer, longlat = T)

##################

library(leaflet)

pal <- colorFactor(c("green", "orange", "red"), domain = c(  "Erzeuger", "Produzent", "Zwischenhaendler"))

leaflet(producersExist) %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 6,
                   stroke = FALSE, fillOpacity = 0.8, color=pal(producersExist$Lieferantentyp),
                   popup = producersExist$Lieferant) %>% 
  addLegend("bottomright", 
            pal = pal, values = ~producersExist$Lieferantentyp,
            title = "Lieferantentyp",
            opacity = 1
  ) %>% 
  addMarkers(Kornkammer, lng = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2], 
             popup= "Kornkammer")


##########################################

Lex <- dbGetQuery(con, "SELECT * FROM productInfo WHERE Lieferant == 'Biohof Lex' AND Produkte_Zusammenfassung IN DISTINCT Produkte_Zusammenfassung")
Lex


########### original database ################
korn <- dbConnect(SQLite(), "data/Backup_example.BAK")

dbListTables(korn)
dbListFields(korn, "accounts")

accountTop <- dbGetQuery(korn, "SELECT * FROM accounts LIMIT 10")

androidTop <- dbGetQuery(korn, "SELECT * FROM android_metadata LIMIT 10")

productsTop <- dbGetQuery(korn, "SELECT * FROM products LIMIT 100")

sessionsTop <- dbGetQuery(korn, "SELECT * FROM sessions LIMIT 10")

sequenceTop <- dbGetQuery(korn, "SELECT * FROM sqlite_sequence LIMIT 100")

transprodTop <- dbGetQuery(korn, "SELECT * FROM transaction_products LIMIT 10")

transactionsTop <- dbGetQuery(korn, "SELECT * FROM transactions LIMIT 10")


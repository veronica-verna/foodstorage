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

productInfo <- dbGetQuery(con, "SELECT * FROM productInfo")
#kopf
#kopf[2,4]

producers <- dbGetQuery(con, "SELECT * FROM producerAdress")
producers$xCoord <- as.numeric(producers$xCoord)
producers$yCoord <- as.numeric(producers$yCoord)

producersExist <- producers[-which(is.na(producers$xCoord)),]

# convert producers to spatialpointsdataframe
coordinates(producersExist) <- ~xCoord + yCoord

#Kornkammer <- data.table(Name = "Kornkammer", Strasse = "Höheweg 3", PLZ= "79104", Ort = "Freiburg")
#Kornkammer[1, xCoord := geocode(paste0(Strasse, ", ", PLZ, " ", Ort))[1]]
#Kornkammer[1, yCoord := geocode(paste0(Strasse, ", ", PLZ, " ", Ort))[2]]
#dbWriteTable(con, "AdresseKornkammer", Kornkammer)

Kornkammer <- dbGetQuery(con, "SELECT * from AdresseKornkammer")
coordinates(Kornkammer) <- ~xCoord + yCoord
crs(Kornkammer) <-  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

crs(producersExist) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

#producersExist$EntfernungKK <- spDistsN1(producersExist, Kornkammer, longlat = T)
#Distances <- data.table(Lieferant = producersExist$Lieferant, EntfernungKK = producersExist$EntfernungKK)

#producers <- as.data.table(left_join(producers, Distances, by="Lieferant"))
# write Distance to KK into the database table producerAdress
#dbWriteTable(con, "producerAdress", producers)

dbDisconnect(con)

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
producers <- as.data.table(producers)
Zwischenhaendler <- producers[Lieferantentyp=="Zwischenhaendler", Lieferant]

productInfo <- as.data.table(productInfo)

# für jeden Zwischenhaendler eine tabelle mit: Produkt, Herkunftsadresse (Strasse, PLZ, Ort) oder Land und xCoord und yCoord
#productOrigin <- data.table(Lieferant = "", Produkt = "", Adresse = "", xCoord="", yCoord="")

pI1 <- productInfo[Lieferant %in% Zwischenhaendler, .(Lieferant, Produkte_Zusammenfassung)]
pI2 <- productInfo[Lieferant2 %in% Zwischenhaendler, .(Lieferant2, Produkte_Zusammenfassung)]
names(pI2) <- names(pI1)

productOrigin <- unique(bind_rows(pI1, pI2))[order(Lieferant),]

write.csv2(productOrigin, "../productOrigin.csv")

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


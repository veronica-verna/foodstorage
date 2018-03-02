

#produzenten <- read.csv2("data/Produzenten.csv")
#dbWriteTable(con, "producerAdress", produzenten)


#Kornkammer <- data.table(Name = "Kornkammer", Strasse = "Höheweg 3", PLZ= "79104", Ort = "Freiburg")
#Kornkammer[1, xCoord := geocode(paste0(Strasse, ", ", PLZ, " ", Ort))[1]]
#Kornkammer[1, yCoord := geocode(paste0(Strasse, ", ", PLZ, " ", Ort))[2]]
#dbWriteTable(con, "AdresseKornkammer", Kornkammer)


#producersExist$EntfernungKK <- spDistsN1(producersExist, Kornkammer, longlat = T)
#Distances <- data.table(Lieferant = producersExist$Lieferant, EntfernungKK = producersExist$EntfernungKK)

#producers <- as.data.table(left_join(producers, Distances, by="Lieferant"))
# write Distance to KK into the database table producerAdress
#dbWriteTable(con, "producerAdress", producers)

#productOrigin <- read.csv2("../productOrigin.csv")
#dbWriteTable(con, "productOrigin", productOrigin)

#dbSendQuery(con, "UPDATE producerAdress SET geometry=MakePoint(xCoord, yCoord, 3857)")

#con <- dbConnect(SQLite(), "../foodstorage/data/kornInfo.sqlite")

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



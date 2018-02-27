library(RSQLite)
library(readr)
library(data.table)

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

dbDisconnect(con)

##################
library(leaflet)

m <- leaflet() %>%
  addTiles() %>%
  addMarkers(producers$xCoord, producers$yCoord)

###############

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


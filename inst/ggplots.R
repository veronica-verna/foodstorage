library(RSQLite)
library(readr)
library(data.table)
library(sf)
library(ggplot2)
library(tmap)
library(rgdal)
library(dplyr)
library(raster)
library(tidyr)
library(foodstorage)
library(ggmap)

############
con <- dbConnect(SQLite(), "data/kornInfo.sqlite")

#dbListTables(con)
#dbListFields(con, "kornumsatz_origin")
#dbListFields(con, "productInfo")
#dbListFields(con, "producerAdress")

productInfo <- dbGetQuery(con, "SELECT * FROM productInfo")
producerAdress <- dbGetQuery(con, "SELECT * FROM producerAdress")
kornumsatz <- dbGetQuery(con, "SELECT * FROM kornumsatz_origin")
origin <- dbGetQuery(con, "SELECT * FROM productOrigin")
Kornkammer <- dbGetQuery(con, "SELECT * from AdresseKornkammer")

productOrigin <- origin

KUperYear <- kornumsatz_perYear(kornumsatz = kornumsatz, productInfo = productInfo)
KU <- KUperYear %>% 
  spread(Jahr, Umsatz) %>% 
  mutate(avg = mean(c(`2016`, `2017`), na.rm = T))
names(KU) <- c("Produkte_Zusammenfassung", "turnover2015", "turnover2016", "turnover2017", "avg.turnover")

originWithDistances <- SupplierDistance(origin, producerAdress)

totalDistances <- totalDistances(origin = origin, producers = producerAdress, productInfo = productInfo)

## count occurance of every product in the table, to split the turnover of the product to the different occurances.
totalDistances <- totalDistances %>% 
  add_count(Produkte_Zusammenfassung) %>% 
  left_join(KU, by = "Produkte_Zusammenfassung") %>% 
  mutate(turnover2015 = turnover2015 / n) %>% 
  mutate(turnover2016 = turnover2016 / n) %>% 
  mutate(turnover2017 = turnover2017 / n) %>% 
  mutate(avg.turnover = avg.turnover / n)


meanDists <- totalDistances %>% 
  group_by(Produktgruppe) %>% 
  summarise(avgDistance = mean(Gesamtentfernung, na.rm=T))


ggplot(meanDists, aes(Produktgruppe, avgDistance, fill= Produktgruppe)) + geom_bar(stat = "identity")


###################################### MIRI:

totDist <- totalDistances %>% 
  mutate(numDist = ntile(Gesamtentfernung, 20))

ggplot(totDist, aes(numDist, Gesamtentfernung)) + geom_point()

turnOver2016 <- totalDistances %>%
  group_by(Produktgruppe)%>%
  summarise(Menge = sum(turnover2016, na.rm = T), Distanz = mean(Gesamtentfernung, na.rm = T))

View(totalDistances)
# ist nicht das was wir wollten, aber auch interessant
ggplot(turnOver2016, aes(Distanz, Menge)) + 
  geom_point(aes(color = Produktgruppe, size = 3)) +
  scale_size(guide = "none") +
  ggtitle("Umsatz 2016")

# Counts statt Menge --> Bezug zum Bewicht fehlt
ggplot(totalDistances, aes(x = Gesamtentfernung, fill = Produktgruppe)) + 
  geom_bar(color = "black", binwidth = 1500) 

#position und bins werden nicht gelesen
ggplot(totalDistances, aes(x = Gesamtentfernung, y = turnover2016, fill = Produktgruppe, color = Produktgruppe)) + 
  geom_bar(position = "stack",stat = "identity") 

ggplot(totalDistances, aes(Gesamtentfernung,  fill = Produktgruppe)) + geom_histogram(binwidth = 1000)

#Problem: Zwei stetige Variablen bildet man eigentlich nicht als Barplot ab, wir könnten durch breaks aus der Distanz "künstlich" eine diskrete Variable machen

# Datensatz totalDistances muss geladen sein mit Spalten turnover und Gesamtentfernung

View(totalDistances)
newtotalDistances <- mutate(totalDistances, "Kathegorie" = NA)

# Damit X (= Distanz) diskret wird, unterteilen wir die Daten manuell in Gruppen

# Möglichkeit 1: Einteilung der Distanzen in 1500 km -Schritten
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung <= 1500] <- "0-1500"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 1500 & newtotalDistances$Gesamtentfernung <= 3000] <- "1500-3000"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 3000 & newtotalDistances$Gesamtentfernung <= 4500] <- "3000-4500"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 4500 & newtotalDistances$Gesamtentfernung <= 6000] <- "4500-6000"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 6000 & newtotalDistances$Gesamtentfernung <= 7500] <- "6000-7500"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 7000 & newtotalDistances$Gesamtentfernung <= 8500] <- "7000-8500"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 8500] <- "> 8500"

# Möglichkeit 2: Distanz immer x 2
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung <= 100] <- "0-100"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 100 & newtotalDistances$Gesamtentfernung <= 200] <- "100-200"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 200 & newtotalDistances$Gesamtentfernung <= 400] <- "200-400"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 400 & newtotalDistances$Gesamtentfernung <= 800] <- "400-800"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 800 & newtotalDistances$Gesamtentfernung <= 1600] <- "800-1600"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 1600 & newtotalDistances$Gesamtentfernung <= 3200] <- "1600-3200"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 3200 & newtotalDistances$Gesamtentfernung <= 6400] <- "3200-6400"
newtotalDistances$Kathegorie[newtotalDistances$Gesamtentfernung > 6400 & newtotalDistances$Gesamtentfernung <= 12800] <- "6400-12800"
newtotalDistances$Kathegorie[is.na(newtotalDistances$Gesamtentfernung) == TRUE] <- "NA"



unique(newtotalDistances$Kathegorie)

# Finally!!
positions <- c("0-100", "100-200", "200-400", "400-800", "800-1600", "1600-3200", "3200-6400", "6400-12800", "NA")

ggplot(newtotalDistances, aes(x = Kathegorie, y = avg.turnover, fill = Produktgruppe)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = positions)+
  labs(title = "Konsumierte Produkte nach Distanz", 
       y = "Umsatz in kg bzw. L",
       x = "Distanz [km]")


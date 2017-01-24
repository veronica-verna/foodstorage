# Sortieren der Daten, Hinzufügen von Spalten, etc. ####
setwd("~/Documents/Studium/Bachelor-Arbeit/R-paket/foodcoop-storage")
kornumsatz <- read.csv("kornumsatz.csv", sep=";")
Position <- 1:nrow(kornumsatz)
kornumsatz <- cbind(Position, kornumsatz)
kornumsatz$Datum <- as.Date(kornumsatz$Datum, format="%d/%m/%Y")
kornumsatz$Produkt <- as.character(kornumsatz$Produkt)

# Laden der zwei wichtigen Funktionen
source("~/Documents/Studium/Bachelor-Arbeit/R-paket/foodcoop-storage/prepare.R")
source("~/Documents/Studium/Bachelor-Arbeit/R-paket/foodcoop-storage/myPlot.R")
source("~/Documents/Studium/Bachelor-Arbeit/R-paket/foodcoop-storage/conv.date.R")
source("~/Documents/Studium/Bachelor-Arbeit/R-paket/foodcoop-storage/look.for.errors.R")

# plotten eines beliebigen Produkts
#myPlot(prepare("Dinkel", what.plotting = "Verzehr"))

# Plot Funktion hat noch viele grafische Parameter wie col_line, oder lty, lwd, col_points ....

### verschiedene Produktnamen, aber gleiches Produkt, zusammengefasst ####
more.than.1.list <- list(Linsen.Braun = c("Linsen Braun", "Braune Linsen", "Tellerlinsen"), 
                         Linsen.Beluga = c("Beluglinsen", "Linsen Beluga"), 
                         Linsen.Rot = c("Rote Linsen", "Rote Linsen Neu"),
                         Hirse.Braun = c("Braunhirse", "Hirse Braun"), 
                         Hirse.Gold = c("Goldhirse", "Goldhirse Neu"),
                         Bohnen.Borlotti = c("Bohnen Borlotti", "Borlottibohnen"), 
                         VollkornreisItalien = c("Arborio Reis", "Rundkornreis"), 
                         BratoelDavert = c("Back-/Bratöl, EU"), 
                         Basmati.Braun = c("Basmati Braun", "brauner Basmati", "Langkornreis Natur"),
                         Basmati.Weiß = c("Basmati Weiß", "weißer Basmati"),
                         Bohnen = c("Bohnen", "Bohnen Borlotti", "Borlottibohnen"), 
                         Drink.Dinkel = c("Dinkel Drink", "Drink Dinkel"),
                         Drink.Hafer = c("Drink Hafer", "Hafer Drink"),
                         Drink.Soja = c("Drink Soja", "Soja Drink"),
                         Getrocknetes.Gemuese = c("Getrocknete Äpfel", "Getrocknete Paprika", "Getrocknetes Gemüse"),
                         Honig = c("Honig", "Sonnenblütenhonig"),
                         Espresso = c("Kaffee Espresso", "Kaffe Espresso"),
                         Kaffee = c("Kaffee Geröstet", "Kaffee Zumba", "Kaffe Zumba"),
                         Nudeln = c("Penne semoltano", "Spirelli"),
                         Rosinen = c("Rosinen", "Weinbeeren"),
                         Zucker = c("Rohrohrzucker", "Rübenzucker", "Rübenzucker Sack"),
                         Samba = c("Samba", "Samba – Schokoaufstrich"),
                         Senfkörner = c("Senfkörner", "Senfkörner Ganz", "Senfkörner Neu"),
                         Sonnenblumenoel = c("Sonnenblumenöl", "Sonnenblumenöl, EU "),
                         Spaghetti = c("Spaghetti", "Spaghetti semoltano"),
                         Spuelmittel.Hand = c("Spülmittel", "Spülmittel Hand"),
                         Tomatenmark = c("Tomatenmark", "Tomaten Mark"),
                         Tomatenpassata = c("Tomatenpassata", "Tomaten Passata"),
                         Waschmittel.Pulver = c("Waschmittel Normal", "Waschmittel Pulver"))

# Erstellen eines Produkts ####
for (i in 1:length(more.than.1.list)) {
  kornumsatz[kornumsatz$Produkt %in% more.than.1.list[[i]],]$Produkt <- names(more.than.1.list)[i]
}
kornumsatz$Produkt <- as.factor(kornumsatz$Produkt)

dif_products <- levels(kornumsatz$Produkt)
VPE_data.frame <- data.frame(Produkt = dif_products, VPE = numeric(length(dif_products)))
VPE_data.frame[c(1,5,6,9,10,12,14,16,18,27,28,30,31,32,38,46,48,49,50,51,59,61,62,66,71,79,80,88,89,94,97),2] <- 25 # Normales Sackgebinde
VPE_data.frame[21,2] <- 150 # Hafer Drink
VPE_data.frame[20,2] <- 100 # Dinkel Drink
VPE_data.frame[22,2] <- 100 # Soja Drink
VPE_data.frame[29,2] <- 75 # Haferflocken
VPE_data.frame[19,2] <- 50 # Buchweizen Drink + Sonnenblumenkerne
VPE_data.frame[c(26,54,55,67,68,69,70,82,86,87),2] <- 30 # getrocknetes Gemüse, Nudeln, Olivenöl, Saft, SB-Öl, Tomatenzeugs
VPE_data.frame[c(83,90,92,93),2] <- 20 # Spüli + Waschmittel
VPE_data.frame[33,2] <- 15 # Honig
VPE_data.frame[c(2,11,23,25,39,52,60,63,81,84,91),2] <- 10 # Öl + Essig + Erdnuss-Mandelmus + Gemüsebrühe + Kokosfett + Rosinen + SpüliMaschine
VPE_data.frame[72,2] <- 8 # Samba
VPE_data.frame[c(4,17,53,65,74,98),2] <- 6 # Aufstriche
VPE_data.frame[c(7,24,35,45,75,77,78), 2] <- 5 # manche Gewürze + Kaffee + Senf
VPE_data.frame[c(3,8,13,15,34,36,37,40,41,42,43,44,47,56,57,58,64,73,76,85,95,96),2] <- 1 # Gewürze

kornumsatz_merged <- merge(kornumsatz, VPE_data.frame, by='Produkt', all = T)
kornumsatz <- kornumsatz_merged[with(kornumsatz_merged, order(Datum, Position)), ]
rm(kornumsatz_merged, VPE_data.frame, dif_products, more.than.1.list)
#########
all <- levels(kornumsatz$Produkt)

#### Aufgesplittet nach Produktart ####
Huelsenfruechte <- c("Beluglinsen", "Bohnen", "Bohnen Borlotti", "Borlottibohnen", "Braune Linsen", "Linsen Beluga", "Linsen Braun", "Rote Linsen", "Rote Linsen Neu", "Tellerlinsen")


#### Aufgesplittet nach Lieferant ####

BioHofLex <- c("Bohnen.Borlotti", "Buchweizen", "Hirse.Braun", "Linsen.Beluga", "Linsen.Braun", "Polenta", "Tellerlinsen")
Luzernenhof <- c("Dinkel", "Hafer", "Roggen", "Weizen")
OBEG <- c("Apfelessig", "Haferflocken", "Kürbiskerne", "Leinsamen", "Rapsöl", "Rübenzucker Sack", "Sonnenblumenkerne", "Sonnenblumenoel") # unsicher bei Kichererbsen 2. Ladung, Rundkornreis, Sesam, Sonnenblumenkerne
HakumaFood <- c("Senf Kirsche", "Senf Sarepta", "Senf Mango")
HofgutStorzeln <- c("Drink Buchweizen", "Drink.Dinkel", "Drink.Hafer", "Drink.Soja")
HofgutUnterbach <- c("Haselnüsse geschält", "Walnüsse")
Erich <- c("Honig")
Cashew4you <- c("Cashews")
Sonett <- as.character(c("Allesreiniger", "Spuelmittel.Hand", "Spülmittel Maschine", "Waschmittel Lavendel", "Waschmittel.Pulver", "Waschmittel Seide/Wolle", "Waschmittel Sensitiv"))
ElephantBeans <- c("Espresso", "Kaffee")
Sonnenobst <- c("Getrocknetes.Gemuese")
Naturata <- c("Gemüsebrühe", "Kokosfett", "Nudeln", "Olivenöl", "Spaghetti", "Tomatenmark", "Tomatenpassata", "Zucker")
Wolfgang <- c("Bohnen", "Saft Apfel", "Saft Apfel-Birne", "Saft Apfel-Möhre", "Saft Trauben")
Grosshandel <- c("Basilikum", "Basitom", "Basmati.Braun", "Basmati.Weiß", "Blaumohn", "BratoelDavert", "Buchweizenmehl", "Couscous", "Currychini", "Erdnussmus", "Grünkern", "Hirse.Gold", "Kichererbsen", "Kräuter der Provence", "Kümmel", "Linsen.Rot", "Mandelmus", "Mepfel", "Oregano", "Paprika Edelsüß", "Risottoreis", "Rosinen", "Rosmarin", "Rote Beete Meerettich", "Salz", "Samba", "Schwarzkümmel", "Sendi", "Senfkörner", "Sesam", "Thymian", "VollkornreisItalien", "Zwiebelschmelz") # SB-Kerne + Zimt Gemahlen, evtl Sesam
SahanaEliya <- c("Bockshornklee Ganz", "Chilli Gemahlen", "Ingwer Gemahlen", "Kaffee roh", "Kardamom Ganz", "Koriander Ganz", "Koriander Gemahlen", "Kreuzkümmel Ganz", "Kreuzkümmel Gemahlen", "Kurkuma Gemahlen", "Pfeffer Schwarz Ganz", "Senfkörner Ganz", "Zimt Ganz", "Zimt Gemahlen")

lieferanten <- list(BioHofLex, Erich, Luzernenhof, OBEG, HakumaFood, HofgutStorzeln, HofgutUnterbach, Sonett, ElephantBeans, Naturata, Sonnenobst, Wolfgang, Großhandel, NepalVerein)

lieferanten_namen <- c(deparse(substitute(BioHofLex)), deparse(substitute(Erich)), deparse(substitute(Luzernenhof)), deparse(substitute(OBEG)), deparse(substitute(HakumaFood)), deparse(substitute(HofgutStorzeln)), deparse(substitute(HofgutUnterbach)), deparse(substitute(Sonett)), deparse(substitute(ElephantBeans)), deparse(substitute(Naturata)), deparse(substitute(Sonnenobst)), deparse(substitute(Wolfgang)), deparse(substitute(Großhandel)), deparse(substitute(NepalVerein)), deparse(substitute(Cashew4you)), deparse(substitute(spanischeKoop)))
verzehr_sum <- numeric()
lager_sum <- numeric()
for (i in 1:length(lieferanten)) {
  verzehr_sum[i] <- sum(prepare(lieferanten[[i]])$data[,4])
  lager_sum[i] <- sum(prepare(lieferanten[[i]])$data[,3])
}
verzehr_sum[15] <- sum(prepare("Cashews", "all")$data[,4])
verzehr_sum[16] <- sum(prepare("Olivenöl", "all")$data[,4])
lager_sum[15] <- sum(prepare("Cashews", "all")$data[,3])
lager_sum[16] <- sum(prepare("Olivenöl", "all")$data[,4])


### nur Großhandel und direkt Produzenten ####
par(mar=c(8,5, 4, 2))
Großhandel <- sum(as.numeric(verzehr_sum[c(8,10,13)])) # Bode, Daver, Naturata, Sonett
DirektErzeuger <- sum(as.numeric(verzehr_sum[-c(8,10,13)])) # alles andere
barplot(c(Großhandel, DirektErzeuger), names.arg = c("Großhandel", "Direkt"), ylab="Verzehr in Kilo", main = "Verzehr nach Lieferanten geordnet", las = 2, ylim = c(0,2000))
legend("topleft", legend=c("Großhandel bestehend aus", "Sonett", "Naturata", "Bode", "Davert"), fill = c("white", "black", "black", "black", "black"))
barplot(verzehr_sum, names.arg = lieferanten_namen, ylab = "Verzehr in Kilo", main = "Verzehr nach Lieferanten geordnet", las = 2)


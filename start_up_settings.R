# Sortieren der Daten, Hinzufügen von Spalten, etc. ####
kornumsatz <- read.csv("~/Documents/Studium/Bachelor-Arbeit/R-paket/foodcoop-storage/kornumsatz.csv", sep=";")
Position <- 1:nrow(kornumsatz)
kornumsatz <- cbind(Position, kornumsatz)
kornumsatz$Datum <- as.Date(kornumsatz$Datum, format="%d/%m/%Y")
kornumsatz$Produkt <- as.character(kornumsatz$Produkt)

# Laden der zwei wichtigen Funktionen
source(prepare.R)
source(myPlot.R)

# plotten eines beliebigen Produkts
myPlot(prepare("Dinkel", what.plotting = "Verzehr"))

# Plot Funktion hat noch viele grafische Parameter wie col_line, oder lty, lwd, col_points ....

### verschiedene Produktnamen, aber gleiches Produkt, zusammengefasst ####
all <- levels(kornumsatz$Produkt)
more.than.1.list <- list(Linsen.Braun <- c("Linsen Braun", "Braune Linsen", "Tellerlinsen"), 
                         Linsen.Beluga <- c("Beluglinsen", "Linsen Beluga"), 
                         Hirse.Braun <- c("Braunhirse", "Hirse Braun"), 
                         Bohnen.Borlotti <- c("Bohnen Borlotti", "Borlottibohnen"), 
                         VollkornreisItalien <- c("Arborio Reis"), 
                         Bratoel <- c("Back-/Bratöl, EU"), 
                         Basmati.Braun <- c("Basmati Braun", "brauner Basmati", "Langkornreis Natur"),
                         Basmati.Weiß <- c("Basmati Weiß", "weißer Basmati"),
                         Bohnen <- c("Bohnen", "Bohnen Borlotti", "Borlottibohnen"), 
                         Drink.Dinkel <- c("Dinkel Drink", "Drink Dinkel"),
                         Drink.Hafer <- c("Drink Hafer", "Hafer Drink"),
                         Drink.Soja <- c("Drink Soja", "Soja Drink"),
                         Getrocknetes.Gemuese <- c("Getrocknete Äpfel", "Getrocknete Paprika", "Getrocknetes Gemüse"),
                         Honig <- c("Honig", "Sonnenblütenhonig"),
                         Espresso <- c("Kaffee Espresso", "Kaffe Espresso"),
                         Kaffee <- c("Kaffee Geröstet", "Kaffee Zumba", "Kaffe Zumba"),
                         Nudeln <- c("Penne semoltano", "Spirelli"),
                         Rosinen <- c("Rosinen", "Weinbeeren"),
                         Zucker <- c("Rohrohrzucker", "Rübenzucker", "Rübenzucker Sack"),
                         Samba <- c("Samba", "Samba - Schokoaufstrich"),
                         Senfkörner <- c("Senfkörner", "Senfkörner Ganz", "Senfkörner Neu"),
                         Sonnenblumenoel <- c("Sonnenblumenöl", "Sonnenblumenöl, EU "),
                         Spaghetti <- c("Spaghetti", "Spaghetti semoltano"),
                         Spuelmittel.Hand <- c("Spülmittel", "Spülmittel Hand"),
                         Tomatenmark <- c("Tomatenmark", "Tomaten Mark"),
                         Tomatenpassata <- c("Tomatenpassata", "Tomaten Passata"),
                         Waschmittel.Pulver <- c("Waschmittel Normal", "Waschmittel Pulver"))

# Erstellen eines Produkts ####
for (i in 1:length(more.than.1.list)) {
  kornumsatz[kornumsatz$Produkt %in% more.than.1.list[i],]$Produkt <- "Linsen.Braun"
}



#### Aufgesplittet nach Produktart ####
Huelsenfruechte <- c("Beluglinsen", "Bohnen", "Bohnen Borlotti", "Borlottibohnen", "Braune Linsen", "Linsen Beluga", "Linsen Braun", "Rote Linsen", "Rote Linsen Neu", "Tellerlinsen")


#### Aufgesplittet nach Lieferant ####

BioHofLex <- c("Beluglinsen", "Bohnen Borlotti", "Borlottibohnen", "Braune Linsen", "Braunhirse", "Buchweizen", "Hirse Braun", "Linsen Beluga", "Linsen Braun", "Polenta", "Tellerlinsen")
Luzernenhof <- c("Dinkel", "Hafer", "Roggen", "Weizen")
OBEG <- c("Apfelessig", "Haferflocken", "Kürbiskerne", "Langkornreis Natur", "Leinsamen", "Rapsöl", "Rübenzucker Sack", "Rundkornreis", "Salz", "Sesam", "Sonnenblumenkerne", "Sonnenblumenöl") # unsicher bei Kichererbsen 2. Ladung, Rundkornreis, Sesam, Sonnenblumenkerne
HakumaFood <- c("Senf Kirsche", "Senf Sarepta", "Senf Mango")
HofgutStorzeln <- c("Dinkel Drink", "Drink Buchweizen", "Drink Dinkel", "Drink Hafer", "Drink Soja", "Hafer Drink", "Soja Drink")
HofgutUnterbach <- c("Haselnüsse geschält", "Walnüsse")
Erich <- c("Honig", "Sonnenblütenhonig")
Cashew4you <- c("Cashews")
Sonett <- as.character(c("Allesreiniger", "Spülmittel", "Spülmittel Hand", "Spülmittel Maschine", "Waschmittel Lavendel", "Waschmittel Normal", "Waschmittel Pulver", "Waschmittel Seide/Wolle", "Waschmittel Sensitiv"))
ElephantBeans <- c("Kaffee Espresso", "Kaffee Geröstet", "Kaffe Espresso", "Kaffee Zumba", "Kaffe Zumba")
Sonnenobst <- c("Getrocknete Äpfel", "Getrocknete Paprika", "Getrocknetes Gemüse")
Naturata <- c("Gemüsebrühe", "Kokosfett", "Olivenöl", "Penne semoltano", "Rübenzucker", "Spaghetti", "Spaghetti semoltano", "Spirelli", "Tomatenmark", "Tomaten Mark", "Tomatenpassata", "Tomaten Passata")
Wolfgang <- c("Bohnen", "Saft Apfel", "Saft Apfel-Birne", "Saft Apfel-Möhre", "Saft Trauben")
Grosshandel <- c("Arborio Reis", "Back-/Bratöl, EU", "Basilikum", "Basitom", "Basmati Braun", "Basmati Weiß", "Blaumohn", "brauner Basmati", "Buchweizenmehl", "Couscous", "Currychini", "Erdnussmus", "Goldhirse", "Goldhirse Neu", "Grünkern", "Kichererbsen", "Kräuter der Provence", "Kümmel", "Mandelmus", "Mepfel", "Oregano", "Paprika Edelsüß", "Risottoreis", "Rohrohrzucker", "Rosinen", "Rosmarin", "Rote Beete Meerettich", "Rote Linsen", "Rote Linsen Neu", "Samba", "Samba - Schokoaufstrich", "Schwarzkümmel", "Sendi", "Senfkörner", "Senfkörner Neu", "Sonnenblumenöl, EU ", "Thymian", "Weinbeeren", "weißer Basmati", "Zimt Gemahlen", "Zwiebelschmelz") # SB-Kerne + Zimt Gemahlen, evtl Sesam
spanischeKoop <- as.character(c("Olivenöl"))
NepalVerein <- c("Bockshornklee Ganz", "Chilli Gemahlen", "Ingwer Gemahlen", "Kaffee roh", "Kardamom Ganz", "Koriander Ganz", "Koriander Gemahlen", "Kreuzkümmel Ganz", "Kreuzkümmel Gemahlen", "Kurkuma Gemahlen", "Pfeffer Schwarz Ganz", "Senfkörner Ganz", "Zimt Ganz", "Zimt Gemahlen")

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


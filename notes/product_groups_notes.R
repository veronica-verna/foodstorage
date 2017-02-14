#### notes - product groups ####
#########
## erstmal alle Produkte ##
alle <- levels(kornumsatz$Produkt)

# müssen nochmal überprüft werden, Stand 27.01.2017:
# Bugs: BratoelDavert, Gemüsebrühe, Getrocknetes Gemüse, Grünkern, Hafer, Roggen
# Bugs: Haselnüsse geschält, Kaffee roh, Mepfel Pfeffer Schwarz Ganz,Polenta, Rote Beete Merretich
# Bugs: Saft Apfel, Saft Apfel-Birne, Saft Apfel-Möhre, Sendi, SEnf MAngo, Sesam, Spaghetti, Spüli Maschine
# Bugs: Waschmittel Lavendel, Waschmittel Sensitiv, Weizen
# sind komisch aus: Buchweizenmehl, Aufstriche, Kaffee, Zwiebelschmelz
# spannend: Cashews
# schön zu sehen: Waschmittel Pulver, 

#### Aufgesplittet nach Produktart ####
## detailiert
Huelsenfruechte <- c("Bohnen", "Bohnen.Borlotti", "Kichererbsen", "Linsen.Braun", "Linsen.Beluga", "Linsen.Rot")
Oelsaaten <- c("Blaumohn", "Leinsamen", "Sonnenblumenkerne", "Kürbiskerne", "Sesam")
Nuesse <- c("Cashews", "Haselnüsse geschält", "Walnüsse")
Gewuerze <- c("Basilikum", "Bockshornklee Ganz", "Chilli Gemahlen", "Gemüsebrühe", "Ingwer Gemahlen", "Kardamom Ganz", "Koriander Ganz", "Koriander Gemahlen", "Kräuter der Provence", "Kreuzkümmel Ganz", "Kreuzkümmel Gemahlen", "Kümmel", "Kurkuma Gemahlen", "Oregano", "Paprika Edelsüß", "Pfeffer Schwarz Ganz", "Rosmarin", "Salz", "Schwarzkümmel", "Senfkörner", "Thymian", "Zimt Ganz", "Zimt Gemahlen")
Putzequipment <- c("Allesreiniger", "Spuelmittel.Hand", "Spülmittel Maschine", "Waschmittel Lavendel", "Waschmittel.Pulver", "Waschmittel Seide/Wolle", "Waschmittel Sensitiv")
Oel.Essig <- c("Apfelessig", "BratoelDavert", "Olivenöl", "Rapsöl", "Sonnenblumenoel")
Reis <- c("Basmati.Braun", "Basmati.Weiss", "Risottoreis", "Rundkornreis")
Getreideprodukte <- c("Buchweizen", "Buchweizenmehl", "Couscous", "Dinkel", "Grünkern", "Hafer", "Haferflocken", "Hirse.Braun", "Hirse.Gold", "Nudeln", "Polenta", "Roggen", "Spaghetti", "Weizen")
Getreidedrinks <- c("Drink Buchweizen", "Drink.Dinkel", "Drink.Hafer", "Drink.Soja")
Aufstriche <- c("Basitom", "Currychini", "Erdnussmus", "Mandelmus", "Mepfel", "Rote Beete Meerettich", "Samba", "Sendi", "Senf Kirsche", "Senf Mango", "Senf Sarepta", "Zwiebelschmelz")
Sonstiges <- c("Espresso", "Getrocknetes.Gemuese", "Honig", "Kaffee", "Kaffee roh", "Kokosfett", "Rosinen", "Tomatenmark", "Tomatenpassata", "Zucker")
Saefte <- c("Saft Apfel", "Saft Apfel-Birne", "Saft Apfel-Möhre", "Saft Trauben")
## mehr zusammengefasst
Oelsaaten.Huelsenfruechte <- c(Huelsenfruechte, Oelsaaten, Nuesse)
Gewuerze
Putzequipment
Oel.Essig
Getreideprodukte <- c(Getreideprodukte, Reis)
Getraenke <- c(Getreidedrinks, Saefte)
Aufstriche
Sonstiges

#### Aufgesplittet nach Lieferant ####

BioHofLex <- c("Bohnen.Borlotti", "Buchweizen", "Hirse.Braun", "Linsen.Beluga", "Linsen.Braun", "Polenta")
Luzernenhof <- c("Dinkel", "Hafer", "Roggen", "Weizen")
OBEG <- c("Apfelessig", "Haferflocken", "Kürbiskerne", "Leinsamen", "Rapsöl", "Rübenzucker Sack", "Sonnenblumenkerne", "Sonnenblumenoel") # unsicher bei Kichererbsen 2. Ladung, Rundkornreis, Sesam, Sonnenblumenkerne
HakumaFood <- c("Senf Kirsche", "Senf Sarepta", "Senf Mango")
HofgutStorzeln <- c("Drink Buchweizen", "Drink.Dinkel", "Drink.Hafer", "Drink.Soja")
HofgutUnterbach <- c("Haselnüsse geschält", "Walnüsse")
Erik <- c("Honig")
Cashew4you <- c("Cashews")
Sonett <- as.character(c("Allesreiniger", "Spuelmittel.Hand", "Spülmittel Maschine", "Waschmittel Lavendel", "Waschmittel.Pulver", "Waschmittel Seide/Wolle", "Waschmittel Sensitiv"))
ElephantBeans <- c("Espresso", "Kaffee")
Sonnenobst <- c("Getrocknetes.Gemuese")
Naturata <- c("Gemüsebrühe", "Kokosfett", "Nudeln", "Olivenöl", "Spaghetti", "Tomatenmark", "Tomatenpassata", "Zucker")
Wolfgang <- c("Bohnen", "Saft Apfel", "Saft Apfel-Birne", "Saft Apfel-Möhre", "Saft Trauben")
Grosshandel <- c("Basilikum", "Basitom", "Basmati.Braun", "Basmati.Weiss", "Blaumohn", "BratoelDavert", "Buchweizenmehl", "Couscous", "Currychini", "Erdnussmus", "Grünkern", "Hirse.Gold", "Kichererbsen", "Kräuter der Provence", "Kümmel", "Linsen.Rot", "Mandelmus", "Mepfel", "Oregano", "Paprika Edelsüß", "Risottoreis", "Rosinen", "Rosmarin", "Rote Beete Meerettich", "Salz", "Samba", "Schwarzkümmel", "Sendi", "Senfkörner", "Sesam", "Thymian", "VollkornreisItalien", "Zwiebelschmelz") # SB-Kerne + Zimt Gemahlen, evtl Sesam
SahanaEliya <- c("Bockshornklee Ganz", "Chilli Gemahlen", "Ingwer Gemahlen", "Kaffee roh", "Kardamom Ganz", "Koriander Ganz", "Koriander Gemahlen", "Kreuzkümmel Ganz", "Kreuzkümmel Gemahlen", "Kurkuma Gemahlen", "Pfeffer Schwarz Ganz", "Senfkörner", "Zimt Ganz", "Zimt Gemahlen")

lieferanten <- c(BioHofLex, Erik, Luzernenhof, OBEG, HakumaFood, HofgutStorzeln, HofgutUnterbach, Sonett, ElephantBeans, Naturata, Sonnenobst, Wolfgang, Bode, SehanaEliya)

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


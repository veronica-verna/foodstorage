# sort data, add columns, adapt product names, etc ####

startup.settings <- function(table, reduce = TRUE) {
  
  
  kornumsatz <- table
  Position <- 1:nrow(kornumsatz)
  kornumsatz <- cbind(Position, kornumsatz)
  kornumsatz$Datum <- as.Date(kornumsatz$Datum, format="%d/%m/%Y")
  kornumsatz$Produkt <- as.character(kornumsatz$Produkt)
  
  
  
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
                           Rundkornreis = c("Arborio Reis", "Rundkornreis"), 
                           BratoelDavert = c("Back-/Bratöl, EU"), 
                           Basmati.Braun = c("Basmati Braun", 
                                             "brauner Basmati", 
                                             "Langkornreis Natur"),
                           Basmati.Weiss = c("Basmati Weiß", "weißer Basmati"),
                           Bohnen = c("Bohnen"), 
                           Drink.Dinkel = c("Dinkel Drink", "Drink Dinkel"),
                           Drink.Hafer = c("Drink Hafer", "Hafer Drink"),
                           Drink.Soja = c("Drink Soja", "Soja Drink"),
                           Getrocknetes.Gemuese = c("Getrocknete Äpfel", 
                                                    "Getrocknete Paprika", 
                                                    "Getrocknetes Gemüse"),
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
  
  # Korrektur Bohnen
  kornumsatz[kornumsatz$Produkt == "Bohnen.Borlotti" & kornumsatz$Datum >= as.Date("2016-10-13", origin="1970-01-01"), ]$Produkt <- "Bohnen"
  
  
  dif_products <- levels(kornumsatz$Produkt)
  VPE_data.frame <- data.frame(Produkt = dif_products, VPE = numeric(length(dif_products)))
  VPE_data.frame[c(1,5,6,9,12,14,16,18,27,28,30,31,32,38,46,48,49,50,51,59,61,62,70,78,79,82,87,88,89,91,92,93,96),2] <- 25 # Normales Sackgebinde
  VPE_data.frame[21,2] <- 12 # Hafer Drink
  VPE_data.frame[20,2] <- 12 # Dinkel Drink
  VPE_data.frame[c(22,85,86),2] <- 12 # Soja Drink, Tomatenzeugs
  VPE_data.frame[29,2] <- 25 # Haferflocken
  VPE_data.frame[19,2] <- 12 # Buchweizen Drink + Sonnenblumenkerne
  VPE_data.frame[c(26,54,55,66,67,68,69,80),2] <- 15 # getrocknetes Gemüse, Nudeln, Olivenöl, Saft, SB-Öl
  VPE_data.frame[33,2] <- 15 # Honig
  VPE_data.frame[c(2,10,11,23,25,39,52,60,63,81,83,90),2] <- 10 # Öl + Essig + Erdnuss-Mandelmus + Gemüsebrühe + Kokosfett + Rosinen + SpüliMaschine
  VPE_data.frame[71,2] <- 8 # Samba
  VPE_data.frame[c(4,17,53,65,73,97),2] <- 6 # Aufstriche
  VPE_data.frame[c(7,24,35,45,72,74,76,77), 2] <- 5 # manche Gewürze + Kaffee + Senf
  VPE_data.frame[c(3,8,13,15,34,36,37,40,41,42,43,44,47,56,57,58,64,75,84,94,95),2] <- 1 # Gewürze
  
  kornumsatz_merged <- merge(kornumsatz, VPE_data.frame, by='Produkt', all = T)
  kornumsatz <- kornumsatz_merged[with(kornumsatz_merged, order(Datum, Position)), ]
  rm(kornumsatz_merged, VPE_data.frame, dif_products, more.than.1.list, i, Position)
  
  # one date, two rows -> reduce them to one
  if (reduce == TRUE) kornumsatz <- reduceONE(kornumsatz)
  return(kornumsatz)
}

shinyServer(function(input, output){
  # read the data
  kornumsatz <- read.csv("data/kornumsatz.csv", sep =";")
  kornumsatz <- startup.settings(kornumsatz)
  
  ## create product groups
  Huelsenfruechte <- c("Bohnen", "Bohnen.Borlotti", "Kichererbsen", "Linsen.Braun", "Linsen.Beluga", "Linsen.Rot")
  Oelsaaten <- c("Blaumohn", "Leinsamen", "Sonnenblumenkerne", "Kürbiskerne", "Sesam")
  Nuesse <- c("Cashews", "Haselnüsse geschält", "Walnüsse")
  Gewuerze <- c("Basilikum", "Bockshornklee Ganz", "Chilli Gemahlen", "Gemüsebrühe", "Ingwer Gemahlen", "Kardamom Ganz", "Koriander Ganz", "Koriander Gemahlen", "Kräuter der Provence", "Kreuzkümmel Ganz", "Kreuzkümmel Gemahlen", "Kümmel", "Kurkuma Gemahlen", "Oregano", "Paprika Edelsüß", "Pfeffer Schwarz Ganz", "Rosmarin", "Schwarzkümmel", "Senfkörner", "Thymian", "Zimt Ganz", "Zimt Gemahlen")
  Putzequipment <- c("Allesreiniger", "Spuelmittel.Hand", "Spülmittel Maschine", "Waschmittel Lavendel", "Waschmittel.Pulver", "Waschmittel Seide/Wolle", "Waschmittel Sensitiv")
  Oel.Essig <- c("Apfelessig", "BratoelDavert", "Olivenöl", "Rapsöl", "Sonnenblumenoel")
  Reis <- c("Basmati.Braun", "Basmati.Weiss", "Risottoreis", "Rundkornreis")
  Getreideprodukte <- c("Buchweizen", "Buchweizenmehl", "Couscous", "Dinkel", "Grünkern", "Hafer", "Haferflocken", "Hirse.Braun", "Hirse.Gold", "Nudeln", "Polenta", "Roggen", "Salz", "Spaghetti", "Weizen")
  Getreidedrinks <- c("Drink Buchweizen", "Drink.Dinkel", "Drink.Hafer", "Drink.Soja")
  Aufstriche <- c("Basitom", "Currychini", "Erdnussmus", "Mandelmus", "Mepfel", "Rote Beete Meerettich", "Samba", "Sendi", "Senf Kirsche", "Senf Mango", "Senf Sarepta", "Zwiebelschmelz")
  Sonstiges <- c("Espresso", "Getrocknetes.Gemuese", "Honig", "Kaffee", "Kaffee roh", "Kokosfett", "Rosinen", "Tomatenmark", "Tomatenpassata", "Zucker")
  Saefte <- c("Saft Apfel", "Saft Apfel-Birne", "Saft Apfel-Möhre", "Saft Trauben")
  ## merging
  Oelsaaten <- c(Oelsaaten, Nuesse)
  Grundnahrungsmittel <- c(Getreideprodukte, Reis)
  Getraenke <- c(Getreidedrinks, Saefte)
  Zusammenfassung <- c(Huelsenfruechte, Oelsaaten, Gewuerze, Putzequipment, Oel.Essig, Getreideprodukte, Getraenke, Aufstriche, Sonstiges)
  
  output$prodPlot  <- renderPlot({
    fun_reg(product = input$product, main_header = input$product)
    #currentStorage(group = input$group)
    #x <- faithful[,2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #hist(x, breaks = bins)
  })
  #output$textPlot <- renderText(input$group)
})
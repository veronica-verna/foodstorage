library(shiny)
library(shinyjs)
library(colourpicker)
library(lubridate)
library(data.table)

######################################################################
###################### read kornumsatz ###############################
######################################################################
#kornumsatz <- read.csv2("https://raw.github.com/frumentum/foodcoop-storage/master/data/kornumsatz.csv", colClasses = "character")
kornumsatz <- read.csv2("data/kornumsatz.csv", colClasses = "character")
kornumsatz$Datum <- as.factor(kornumsatz$Datum)
kornumsatz$MengeKum <- as.numeric(kornumsatz$MengeKum)
kornumsatz$Einheit <- as.factor(kornumsatz$Einheit)
kornumsatz$Preis <- as.numeric(kornumsatz$Preis)
kornumsatz$Produkt <- as.factor(kornumsatz$Produkt)
kornumsatz$Umsatz <- as.numeric(kornumsatz$Umsatz)
kornumsatz$Bestand_Einheit <- as.numeric(kornumsatz$Bestand_Einheit)
kornumsatz$Bestand_Preis <- as.numeric(kornumsatz$Bestand_Preis)
kornumsatz <- startup.settings(kornumsatz)

##################################### product groups ######################
product.group <- list("Bitte wählen" = "Bitte waehlen",
                      "Zusammenfassung", 
                      "Grundnahrungsmittel",
                      "Hülsenfrüchte" = "Huelsenfruechte", 
                      "Ölsaaten" = "Oelsaaten", 
                      "Gewürze" = "Gewuerze", 
                      "Putzequipment",
                      "Öl und Essig" = "Oel.Essig", 
                      "Getränke" = "Getraenke", 
                      "Aufstriche",
                      "Sonstiges")
groups.long <- list("Grundnahrungsmittel" = c("Buchweizen", "Buchweizenmehl", "Couscous", "Dinkel", "Grünkern", "Hafer", "Haferflocken", "Hirse.Braun", "Hirse.Gold", "Nudeln", "Polenta", "Roggen", "Salz", "Spaghetti", "Weizen", "Basmati.Braun", "Basmati.Weiss", "Risottoreis", "Rundkornreis"),
                    "Huelsenfruechte" = c("Bohnen", "Bohnen.Borlotti", "Kichererbsen", "Linsen.Braun", "Linsen.Beluga", "Linsen.Rot"),
                    "Oelsaaten" = c("Blaumohn", "Leinsamen", "Sonnenblumenkerne", "Kürbiskerne", "Sesam", "Cashews", "Haselnüsse geschält", "Walnüsse"),
                    "Gewuerze" = c("Basilikum", "Bockshornklee Ganz", "Chilli Gemahlen", "Gemüsebrühe", "Ingwer Gemahlen", "Kardamom Ganz", "Koriander Ganz", "Koriander Gemahlen", "Kräuter der Provence", "Kreuzkümmel Ganz", "Kreuzkümmel Gemahlen", "Kümmel", "Kurkuma Gemahlen", "Oregano", "Paprika Edelsüß", "Pfeffer Schwarz Ganz", "Rosmarin", "Schwarzkümmel", "Senfkörner", "Thymian", "Zimt Ganz", "Zimt Gemahlen"),
                    "Oel.Essig" = c("Apfelessig", "BratoelDavert", "Olivenöl", "Rapsöl", "Sonnenblumenoel"),
                    "Putzequipment" = c("Allesreiniger", "Spuelmittel.Hand", "Spülmittel Maschine", "Waschmittel Lavendel", "Waschmittel.Pulver", "Waschmittel Seide/Wolle", "Waschmittel Sensitiv"),
                    "Getraenke" = c("Drink Buchweizen", "Drink.Dinkel", "Drink.Hafer", "Drink.Soja", "Saft Apfel", "Saft Apfel-Birne", "Saft Apfel-Möhre", "Saft Trauben"),
                    "Aufstriche" = c("Basitom", "Currychini", "Erdnussmus", "Mandelmus", "Mepfel", "Rote Beete Meerettich", "Samba", "Sendi", "Senf Kirsche", "Senf Mango", "Senf Sarepta", "Zwiebelschmelz"),
                    "Sonstiges" = c("Espresso", "Getrocknetes.Gemuese", "Honig", "Kaffee", "Kaffee roh", "Kokosfett", "Rosinen", "Tomatenmark", "Tomatenpassata", "Zucker"),
                    "Zusammenfassung" = c("Buchweizen", "Buchweizenmehl", "Couscous", "Dinkel", "Grünkern", "Hafer", "Haferflocken", "Hirse.Braun", "Hirse.Gold", "Nudeln", "Polenta", "Roggen", "Salz", "Spaghetti", "Weizen", "Basmati.Braun", "Basmati.Weiss", "Risottoreis", "Rundkornreis", "Bohnen", "Bohnen.Borlotti", "Kichererbsen", "Linsen.Braun", "Linsen.Beluga", "Linsen.Rot", "Blaumohn", "Leinsamen", "Sonnenblumenkerne", "Kürbiskerne", "Sesam", "Cashews", "Haselnüsse geschält", "Walnüsse", "Basilikum", "Bockshornklee Ganz", "Chilli Gemahlen", "Gemüsebrühe", "Ingwer Gemahlen", "Kardamom Ganz", "Koriander Ganz", "Koriander Gemahlen", "Kräuter der Provence", "Kreuzkümmel Ganz", "Kreuzkümmel Gemahlen", "Kümmel", "Kurkuma Gemahlen", "Oregano", "Paprika Edelsüß", "Pfeffer Schwarz Ganz", "Rosmarin", "Schwarzkümmel", "Senfkörner", "Thymian", "Zimt Ganz", "Zimt Gemahlen", "Apfelessig", "BratoelDavert", "Olivenöl", "Rapsöl", "Sonnenblumenoel", "Allesreiniger", "Spuelmittel.Hand", "Spülmittel Maschine", "Waschmittel Lavendel", "Waschmittel.Pulver", "Waschmittel Seide/Wolle", "Waschmittel Sensitiv", "Drink Buchweizen", "Drink.Dinkel", "Drink.Hafer", "Drink.Soja", "Saft Apfel", "Saft Apfel-Birne", "Saft Apfel-Möhre", "Saft Trauben", "Basitom", "Currychini", "Erdnussmus", "Mandelmus", "Mepfel", "Rote Beete Meerettich", "Samba", "Sendi", "Senf Kirsche", "Senf Mango", "Senf Sarepta", "Zwiebelschmelz", "Espresso", "Getrocknetes.Gemuese", "Honig", "Kaffee", "Kaffee roh", "Kokosfett", "Rosinen", "Tomatenmark", "Tomatenpassata", "Zucker"))



########### Parameters of fun_reg #############
data.list <- list("Von" = "from", "Bis" = "to")
data.pars <- c("from", "to")
graphic.list <- list("Plot-Typ" = "type", "Punktart" = "pch", "Ausrichtung y-Achse" = "las", "Liniendicke" = "lwd", "Linientyp" = "lty")
graphic.pars <- c("type", "pch", "las", "lwd", "lty")
scription.list <- list("Überschrift" = "main_header", "x-Achse" = "xlab", "y-Achse" = "ylab")
scription.pars <- c("main_header", "xlab", "ylab")
col.list <- list("Punkte" = "col_points", "Zukunft-Mitte" = "col_reg", "Konvidenzintervalle" = "col_conv", "Farbe-4-Wochen" = "col_20", "Vergangenheit" = "col_past")
col.pars <- c("col_points", "col_reg", "col_conv", "col_20", "col_past")
which.smoother <- c("smoother")
smoother.pars <- c("span", "degree")
advanced.pars <- c("nec.dates", "more.than", which.smoother, smoother.pars)
advanced.list <- c("Notwendige Datenanzahl" = "nec.dates", "Mindestens" = "more.than", "smoother" = "smoother", "span" = "span", "Grad" = "degree")
fun_reg.list <- list("Zeitangaben" = "data.pars", 
                     "Graphische Parameter" = "graphic.pars",
                     "Beschriftung" = "scription.pars",
                     "Farben" = "col.pars",
                     "Für Fortgeschrittene" = "advanced.pars")
fun_reg.pars <- c(data.pars, graphic.pars, scription.pars, col.pars, advanced.pars)


###############################################################################################
################################## Shiny UI ###################################################
###############################################################################################
ui <- shinyUI(fluidPage(
    titlePanel("Kornkammer"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        ###################### stepwise opening parameter settings ######################
        radioButtons("what", 
                     label = "Aktuell  oder Zukunft?",
                     choices = list("Aktueller Warenbestand" = 'present', 
                                    "Zukunftsprognose" = 'future')),
        
        ################################# Future ########################################
        conditionalPanel(condition = "input.what == 'future'",
                         selectInput("numFuture",
                                     "Einzelnes Produkt oder eine Gruppe?",
                                     choices = list("Bitte wählen" = 0,
                                                    "Einzelnes Produkt" = 1,
                                                    "Produktgruppe (Familie)" = 2,
                                                    "Lieferanten" = 3)),
                         # Which product?
                         conditionalPanel(condition = "input.numFuture == 1",
                                          selectizeInput("productFuture",
                                                         "Produkt",
                                                         choices = c("Bitte wählen" = "Bitte waehlen", 
                                                                     levels(kornumsatz$Produkt)), 
                                                         select = "Bitte waehlen",
                                                         options = list(create = TRUE,
                                                                        placeholder = lapply(levels(kornumsatz$Produkt), '['),
                                                                        maxItems = 1))),
                         conditionalPanel(condition = "input.numFuture == 2",
                                          selectInput("groupFuture",
                                                      "Produktgruppen",
                                                      choices = product.group),
                                          conditionalPanel(condition = "input.groupFuture != 'Bitte waehlen'",
                                                           helpText("Welchen Produkten gehen in den nächsten x-Wochen aus?"),
                                                           numericInput('weeks', 'Wochen', value = 4, min = 1)))
                         ),
        ################################ Present Stock #################################
        conditionalPanel(condition = "input.what == 'present'",
                         selectInput("numPresent",
                                     "Nach Gruppe oder Lieferant?",
                                     choices = list("Bitte wählen" = 0,
                                                    "Einzelnes Produkt" = 1,
                                                    "Produktgruppe (Familie)" = 2,
                                                    "Lieferanten" = 3)),
                         # Which product?
                         conditionalPanel(condition = "input.numPresent == 1",
                                          selectizeInput("productPresent",
                                                         "Produkt",
                                                         choices = c("Bitte wählen" = "Bitte waehlen", 
                                                                     levels(kornumsatz$Produkt)), 
                                                         select = "Bitte waehlen",
                                                         options = list(create = TRUE,
                                                                        placeholder = lapply(levels(kornumsatz$Produkt), '['),
                                                                        maxItems = 1))),
                         conditionalPanel(condition = "input.numPresent == 2",
                                          selectInput("groupPresent",
                                                      "Produktgruppen",
                                                      choices = product.group))
                         ),
        
        conditionalPanel(condition = "input.numFuture != 0 | input.numPresent != 0",
                         checkboxInput("settings",
                                       label = "Erweiterte Einstellungen",
                                       value = FALSE),
                         conditionalPanel(condition = "input.settings == true",
                                          checkboxGroupInput("pars", label ="Parametergruppen",
                                                             choices = fun_reg.list),
                                          conditionalPanel(condition = "input.pars.includes('data.pars')",
                                                           checkboxGroupInput("data", "Zeitangaben", data.list)),
                                          conditionalPanel(condition = "input.pars.includes('graphic.pars')",
                                                           checkboxGroupInput("graphics", "Grafikeinstellungen", graphic.list)),
                                          conditionalPanel(condition = "input.pars.includes('scription.pars')",
                                                           checkboxGroupInput('labels', "Beschriftung", scription.list)),
                                          conditionalPanel(condition = "input.pars.includes('col.pars')",
                                                           checkboxGroupInput('cols', "Farben", col.list)),
                                          conditionalPanel(condition = "input.pars.includes('advanced.pars')",
                                                           checkboxGroupInput('advanced', "Fortgeschritten", advanced.list)),
                                          actionButton("submit", "Übernehmen")
                         )),
        ##################### Parameters: Data ######################################################
        conditionalPanel(condition = "input.data.includes('from')",
                         dateInput("from", "Von", value = as.character(Sys.Date() %m-% months(6)))),
        conditionalPanel(condition = "input.data.includes('to')",
                         dateInput("to", "Bis", value = as.character(Sys.Date()))),
        ##################### Parameters: Graphics ##################################################
        conditionalPanel(condition = "input.graphics.includes('type')",
                         radioButtons("type", "Plot-Art", choices = c("Punkte und Linie" = "b", "Nur Punkte" = "p", "Nur Linie" = "l"), selected = "b")),
#        conditionalPanel(condition = "input.graphics.includes('pch')",
#                         ...),
        conditionalPanel(condition = "input.graphics.includes('las')",
                         numericInput('las', 'Ausrichtung Beschriftung', min = 0, max = 2, step = 1, value = 2)),
#        conditionalPanel(condition = "input.graphics.includes('lwd')",
#                         ...),
#        conditionalPanel(condition = "input.graphics.includes('lty')",
#                         ...)
        #################### Parameters: Labels #####################################################
### here there are '???': default value shall be the name of product = input.productFuture
        conditionalPanel(condition = "input.labels.includes('main_header')",
                         helpText("Nur den Namen des Produktes eingeben im 'Dativ'"),
                         textInput("main_header", "Überschrift")),
        conditionalPanel(condition = "input.labels.includes('xlab')",
                         textInput("xlab", "x-Achsen-Beschriftung", value = "")),
        conditionalPanel(condition = "input.labels.includes('ylab')",
                         textInput("ylab", "y-Achsen-Beschriftung", value = "Warenbestand in Kilo")),
        #################### Parameters: Colours ####################################################
        conditionalPanel(condition = "input.cols.includes('col_points')",
                         colourInput("col_points", "Punktfarbe", value = "grey")),
        conditionalPanel(condition = "input.cols.includes('col_reg')",
                         colourInput("col_reg", "Farbe der 'Zukunftslinie'", value = "grey")),
        conditionalPanel(condition = "input.cols.includes('col_conv')",
                         colourInput("col_conv", "Konvergenzintervalle", value = "lightgrey")),
        conditionalPanel(condition = "input.cols.includes('col_20')",
                         colourInput("col_20", "Warnfarbe", value = "red")),
        conditionalPanel(condition = "input.cols.includes('col_past')",
                         colourInput("col_past", "Vergangenheitslinie", value = "black")),
        #################### Parameters: Advanced ###################################################
        conditionalPanel(condition = "input.advanced.includes('nec.dates')",
                         helpText("Wie viele Daten brauche ich mindestens, um die Zukunft 'vorherzusagen'?"),
                         numericInput("nec.dates", "Notwendige Datenanzahl", min = 5, max = 20, value = 10)),
        conditionalPanel(condition = "input.advanced.includes('more.than')",
                         helpText("Wie viele Tage hintereinander muss der Warenbestand unter 5% des Ausgangsbestands sein, damit der Bestand auf null korigiert wird?"),
                         numericInput("more.than", "mindestens", min = 10, max = 30, value = 15)),
        conditionalPanel(condition = "input.advanced.includes('smoother')",
                         selectInput("smoother", "smoother", which.smoother, selected = "smoother")),
        conditionalPanel(condition = "input.advanced.includes('span')",
                         sliderInput("span", "Intervallbreite", min = 0.01, max = 1, step = 0.01, value = 0.1)),
        conditionalPanel(condition = "input.advanced.includes('degree')",
                         numericInput("degree", "Grad", min = 1, max = 5, value = 1))
      ),
      
      
      mainPanel(
        conditionalPanel(condition = "input.productFuture != 'Bitte waehlen'",
                         plotOutput("prodPlot")),
        conditionalPanel(condition = "input.groupFuture != 'Bitte waehlen'",
                         plotOutput("groupStock", height = 800),
                         tableOutput("orderTable"))
      )
    )
    
  ))

######################################################################################################################
####################################### Shiny Server #################################################################
######################################################################################################################


server <- shinyServer(function(input, output){
    observeEvent(input$submit, {
      output$prodPlot  <- renderPlot({
        fun_reg(product = input$productFuture, main_header = input$productFuture)
      })
    }) 
    output$prodPlot  <- renderPlot({
      if (input$productFuture != 'Bitte waehlen' && input$settings == FALSE) {
        fun_reg(product = input$productFuture, main_header = input$productFuture)
      }
    })
    
    output$groupStock <- renderPlot({
      
      if (input$what == 'future' && input$groupFuture != 'Bitte waehlen') {
        group <- unlist(groups.long[which(names(groups.long) == input$groupFuture)])
        names(group) <- c()
        if (length(group) <= 6) group_reg(group = group, weeks = input$weeks)
      }
    })
    
    output$orderTable <- renderTable({
      if (input$what == 'future' && input$groupFuture != 'Bitte waehlen') {
        group <- unlist(groups.long[which(names(groups.long) == input$groupFuture)])
        names(group) <- c()
        if (length(group) > 6) group_reg(group = group, weeks = input$weeks, list = TRUE)[[1]]
      }
    })
    
  })

shinyApp(ui, server)
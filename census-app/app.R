library(shiny)
library(shinyjs)
library(colourpicker)
library(lubridate)
library(data.table)

######################################################################
###################### read kornumsatz ###############################
######################################################################
#kornumsatz <- read.csv2("data/kornumsatz.csv")
#kornumsatz <- startup.settings(kornumsatz)

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
        
        ############## Zukunftsprognose ########################################
        conditionalPanel(condition = "input.what == 'future'",
                         selectInput("number",
                                     "Einzelnes Produkt oder eine Gruppe?",
                                     choices = list("Bitte wählen" = 0,
                                                    "Einzelnes Produkt" = 1,
                                                    "Produktgruppe (Familie)" = 2,
                                                    "Lieferanten" = 3)),
                         # Which product?
                         conditionalPanel(condition = "input.number == 1",
                                          selectizeInput("product",
                                                         "Produkt",
                                                         choices = c("Bitte wählen" = "Bitte waehlen", 
                                                                     levels(kornumsatz$Produkt)), 
                                                         select = "Bitte waehlen",
                                                         options = list(create = TRUE,
                                                                        placeholder = lapply(levels(kornumsatz$Produkt), '['),
                                                                        maxItems = 1))),
                         conditionalPanel(condition = "input.number == 2",
                                          selectInput("groupFuture",
                                                      "Produktgruppen",
                                                      choices = product.group)),
                         conditionalPanel(condition = "input.groupFuture != 'Bitte waehlen'",
                                          helpText("Welchen Produkten gehen in den nächsten x-Wochen aus?"),
                                          numericInput('weeks', 'Wochen', value = 4, min = 1))),
        #################### Aktueller Warenbestand #################################
        conditionalPanel(condition = "input.what == 'present'",
                         selectInput("numbStock",
                                     "Nach Gruppe oder Lieferant?",
                                     choices = list("Bitte wählen" = 0,
                                                    "Produktgruppe" = 1,
                                                    "Lieferant" = 2)),
                         conditionalPanel(condition = "input.numbStock == 1",
                                          selectInput("groupPresent",
                                                      "Produktgruppen",
                                                      choices = product.group))),
        
        conditionalPanel(condition = "input.number != 0 | input.numbStock != 0",
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
                                                           checkboxGroupInput('scription', "Beschriftung", scription.list)),
                                          conditionalPanel(condition = "input.pars.includes('col.pars')",
                                                           checkboxGroupInput('cols', "Farben", col.list)),
                                          actionButton("submit", "Übernehmen")
                         )),
        conditionalPanel(condition = "input.data.includes('from')",
                         dateInput("from", "Von", value = as.character(Sys.Date() %m-% months(6)))),
        conditionalPanel(condition = "input.cols.includes('col_points')",
                         colourInput("col_points", "Farbe der Punkte", value = "lightgrey"))
      ),
      
      
      mainPanel(
        plotOutput("prodPlot"),
        plotOutput("groupStock")
      )
    )
    
  ))

######################################################################################################################
####################################### Shiny Server #################################################################
######################################################################################################################


server <- shinyServer(function(input, output){
    observeEvent(input$submit, {
      output$prodPlot  <- renderPlot({
        fun_reg(product = input$product, main_header = input$product)
      })
    }) 
    output$prodPlot  <- renderPlot({
      if (input$product != 'Bitte waehlen' && input$settings == FALSE) {
        fun_reg(product = input$product, main_header = input$product)
      }
    })
    
    output$groupStock <- renderPlot({
      if (input$what == 'future' && input$groupFuture != 'Bitte waehlen') {
        group <- unlist(groups.long[which(names(groups.long) == input$groupFuture)])
        names(group) <- c()
        group_reg(group = group, weeks = input$weeks)
      }
    })
    
  })

shinyApp(ui, server)
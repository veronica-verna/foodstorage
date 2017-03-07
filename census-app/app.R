library(shiny)
library(shinyjs)
library(shinythemes)
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
                    "Sonstiges" = c("Espresso", "Getrocknetes.Gemuese", "Honig", "Kaffee", "Kaffee roh", "Kokosfett", "Rosinen", "Tomatenmark", "Tomatenpassata", "Zucker"))



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
fun_reg.list <- list("Zeitangaben" = "data", 
                     "Graphische Parameter" = "graphics",
                     "Beschriftung" = "labels",
                     "Farben" = "col",
                     "Für Fortgeschrittene" = "advanced")
fun_reg.pars <- c(data.pars, graphic.pars, scription.pars, col.pars, advanced.pars)


###############################################################################################
################################## Shiny UI ###################################################
###############################################################################################
ui <- shinyUI(navbarPage("Kornkammer",
                         theme = shinytheme("united"),
                
                         ################################ Warenbestand ############################################
                
                tabPanel("Warenbestand", value = 1,
                         ########################### Header ######################################
                         h2("Du willst wissen, was in der Kornkammer gerade vorrätig ist?"),
                         ########################### Basic Settings ##############################
                         fluidRow(
                           column(4,
                                  selectInput("quantity",
                                              "Einzelnes Produkt oder Gruppe?",
                                              choices = list("Zusammenfassung" = 0,
                                                             "Einzelnes Produkt" = 1,
                                                             "Produktgruppe (Familie)" = 2,
                                                             "Lieferanten" = 3))
                                  ),
                           column(4,
                                  # Which product?
                                  conditionalPanel(condition = "input.quantity == 1",
                                                   selectizeInput("product",
                                                                  "Produkt",
                                                                  choices = c("Bitte wählen" = "Bitte waehlen", 
                                                                              levels(kornumsatz$Produkt)), 
                                                                  select = "Bitte waehlen",
                                                                  options = list(create = TRUE,
                                                                                 placeholder = lapply(levels(kornumsatz$Produkt), '['),
                                                                                 maxItems = 1))),
                                  conditionalPanel(condition = "input.quantity == 2",
                                                   selectInput("group",
                                                               "Produktgruppen",
                                                               choices = product.group)),
                                  conditionalPanel(condition = "input.quantity != 0",
                                                   checkboxInput("settings",
                                                                 label = "Erweiterte Einstellungen",
                                                                 value = FALSE))
                           ),
                           column(4,
                                  conditionalPanel(condition = "input.settings == true",
                                                   checkboxGroupInput("pars", label ="Parametergruppen",
                                                                      choices = fun_reg.list))
                           )
                         ),
                         
                         #########################################################################################
                         ###################### Parameter settings ###############################################
                         #########################################################################################
                         fluidRow(
                           
                           
                           ##################### Parameters: Data and Labels ##########################################
                           column(3,
                                  conditionalPanel(condition = "input.pars.includes('data')",
                                                   checkboxGroupInput("data", "Zeitangaben", data.list)),
                                  conditionalPanel(condition = "input.pars.includes('labels')",
                                                   checkboxGroupInput('labels', "Beschriftung", scription.list)),
                                  conditionalPanel(condition = "input.data.includes('from')",
                                                   dateInput("from", "Von", value = as.character(Sys.Date() %m-% months(6)))),
                                  conditionalPanel(condition = "input.data.includes('to')",
                                                   dateInput("to", "Bis", value = as.character(Sys.Date()))),
                                  conditionalPanel(condition = "input.labels.includes('main_header')",
                                                   helpText("Nur den Namen des Produktes eingeben im 'Dativ'"),
                                                   textInput("main_header", "Überschrift")),
                                  conditionalPanel(condition = "input.labels.includes('xlab')",
                                                   textInput("xlab", "x-Achsen-Beschriftung", value = "")),
                                  conditionalPanel(condition = "input.labels.includes('ylab')",
                                                   textInput("ylab", "y-Achsen-Beschriftung", value = "Warenbestand in Kilo"))
                           ),
                           ##################### Parameters: Graphics ##################################################
                           column(3,
                                  
                                  conditionalPanel(condition = "input.pars.includes('graphics')",
                                                   checkboxGroupInput("graphics", "Grafikeinstellungen", graphic.list)),
                                  
                                  br(),
                                  
                                  conditionalPanel(condition = "input.graphics.includes('type')",
                                                   radioButtons("type", "Plot-Art", choices = c("Punkte und Linie" = "b", "Nur Punkte" = "p", "Nur Linie" = "l"), selected = "b")),
                                  #        conditionalPanel(condition = "input.graphics.includes('pch')",
                                  #                         ...),
                                  conditionalPanel(condition = "input.graphics.includes('las')",
                                                   numericInput('las', 'Ausrichtung Beschriftung', min = 0, max = 2, step = 1, value = 2))
                                  #        conditionalPanel(condition = "input.graphics.includes('lwd')",
                                  #                         ...),
                                  #        conditionalPanel(condition = "input.graphics.includes('lty')",
                                  #)
                           ),
                           #################### Parameters: Colours ####################################################
                           column(3,
                                  conditionalPanel(condition = "input.pars.includes('col')",
                                                   checkboxGroupInput('cols', "Farben", col.list)),
                                  br(),
                                  conditionalPanel(condition = "input.cols.includes('col_points')",
                                                   colourInput("col_points", "Punktfarbe", value = "grey")),
                                  conditionalPanel(condition = "input.cols.includes('col_reg')",
                                                   colourInput("col_reg", "Farbe der 'Zukunftslinie'", value = "grey")),
                                  conditionalPanel(condition = "input.cols.includes('col_conv')",
                                                   colourInput("col_conv", "Konvergenzintervalle", value = "lightgrey")),
                                  conditionalPanel(condition = "input.cols.includes('col_20')",
                                                   colourInput("col_20", "Warnfarbe", value = "red")),
                                  conditionalPanel(condition = "input.cols.includes('col_past')",
                                                   colourInput("col_past", "Vergangenheitslinie", value = "black"))
                           ),
                           #################### Parameters: Advanced ###################################################
                           column(3,
                                  conditionalPanel(condition = "input.pars.includes('advanced')",
                                                   checkboxGroupInput('advanced', "Fortgeschritten", advanced.list)),
                                  br(),
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
                           )
                         ),
                         
                         ############################### Submit Button #################################################
                         fluidRow(
                           conditionalPanel(condition = "input.pars.includes('data') |
                              input.pars.includes('graphics') |
                              input.pars.includes('labels') |
                              input.pars.includes('col') |
                              input.pars.includes('advanced')",
                                                   actionButton("goButton", "Übernehmen"))
                         ),
                         
                         
                         
                         ##############################################################################################
                         ############################# Main Panel #####################################################
                         ##############################################################################################
                         
                         fluidRow(
                           conditionalPanel(condition = "input.quantity == 0",
                                            plotOutput("currentStorageSum", height = 1000)),
                           conditionalPanel(condition = "input.product != 'Bitte waehlen'",
                                            plotOutput("myPlot")),
                           conditionalPanel(condition = "input.group != 'Bitte waehlen'",
                                            plotOutput("currentStorage"))
                         )  
                         ),
                
                ################################# Zukunftsprognose ################################################
                
                tabPanel("Zukunftsprognose", value = 2,
                         ########################### Header ######################################
                         h2("Du möchtest wissen, was als nächstes nachbestellt werden sollte?"),
                         h5(em("Für manche Produkte kann aus verschiedenen Gründen keine Zukunftsprognose gemacht werden. Diese sind gekennzeichnet mit 'manuell überprüfen'. In diesen Fällen schaut euch einfach die Warenbestandskurve an.")),
                         br(),
                         ########################### Basic Settings ##############################
                         fluidRow(
                           column(4,
                                  selectInput("quantityFut",
                                              "Einzelnes Produkt oder Gruppe?",
                                              choices = list("Zusammenfassung" = "sumFut", 
                                                             "Einzelnes Produkt" = "ONEprodFut",
                                                             "Produktgruppe (Familie)" = "familyFut",
                                                             "Lieferanten" = "producerFut"))
                                  ),
                           column(4,
                                  # Which product?
                                  uiOutput("quantityFut")
                                  ),
                           column(4,
                                  conditionalPanel(condition = "output.groupsize > 6",
                                                   uiOutput("weeksFut"))
                           )
                         ),
                         
                         
                         ##############################################################################################
                         ############################# Main Panel #####################################################
                         ##############################################################################################
                         
                         fluidRow(
                           ####################### summary #########################################
                           conditionalPanel(condition = "input.quantityFut == 'sumFut'",
                                            dataTableOutput("group_regSUM")),
                           ####################### for one product #################################
                           conditionalPanel(condition = "input.quantityFut == 'ONEprodFut' && input.productFut != 'Bitte waehlen'",
                                            plotOutput("fun_reg")),
                           ####################### plots for group of products #####################
                           conditionalPanel(condition = "output.groupsize <= 6 && 
                                            input.quantityFut == 'familyFut' && 
                                            input.groupFut != 'Bitte waehlen'",
                                            plotOutput("group_regPlot", height = 1000)),
                           ####################### data.table for group of products ################
                           conditionalPanel(condition = "output.groupsize > 6 && 
                                            input.quantityFut == 'familyFut' && 
                                            input.groupFut != 'Bitte waehlen'",
                                            dataTableOutput("group_regTab")),
                           ####################### only necessary for panels above #################
                           conditionalPanel(condition = "input.quantityFut == 'familyFut' && 
                                            input.groupFut != 'Bitte waehlen'",
                                            textOutput("groupsize"))
                         )
                         )
    
  ))


######################################################################################################################
####################################### Shiny Server #################################################################
######################################################################################################################


server <- shinyServer(function(input, output){
  #################################### Basig Parameters #############################################
  output$quantityFut <- renderUI({
    switch(input$quantityFut,
           "sumFut" = list(helpText("Welche Produkten gehen in den nächsten x-Wochen aus?"),
                           numericInput("summaryFut", "Wochen", value = 4)),
           "ONEprodFut" = selectizeInput("productFut",
                                         "Produkt",
                                         choices = c("Bitte wählen" = "Bitte waehlen", 
                                                     levels(kornumsatz$Produkt)), 
                                         select = "Bitte waehlen",
                                         options = list(create = TRUE,
                                                        placeholder = lapply(levels(kornumsatz$Produkt), '['),
                                                        maxItems = 1)),
           "familyFut" = selectInput("groupFut",
                                     "Produktgruppen",
                                     choices = product.group)
           )
  })
  #################################### fun_reg #####################################################
  
  output$fun_reg  <- renderPlot({
    if (input$quantityFut == 'ONEprodFut') {
      if (input$productFut != 'Bitte waehlen') {
        fun_reg(product = input$productFut, main_header = input$productFut)
      }
    }
  })
  
  #################################### myPlot ########################################################
  observeEvent(input$goButton, {
    output$myPlot  <- renderPlot({
      if (input$product != 'Bitte waehlen') {
        suppressWarnings(myPlot(prepare(input$product, what.plotting = "Warenbestand", myPlot = TRUE)))
      }
    })
  })
  
  output$myPlot <- renderPlot({
    if (input$product != 'Bitte waehlen' && input$settings == FALSE) {
      suppressWarnings(myPlot(prepare(input$product, what.plotting = "Warenbestand", myPlot = TRUE)))
    }
  })
  
  ################################### group_reg ###################################################
  
  ### summary
  output$group_regSUM <- renderDataTable({
    if (input$quantityFut == 'sumFut') {
      big.list <- lapply(groups.long, group_reg, list = T, weeks = input$summaryFut)
      df <- do.call("rbind", big.list)
      return(df[with(df, order(df[,2], df[,3])),])
    }
  })
  
  ### checking length of group
  output$groupsize <- reactive({
    if (input$quantityFut == 'familyFut') {
      if (input$groupFut != "Bitte waehlen") {
        group <- unlist(groups.long[which(names(groups.long) == input$groupFut)])
        names(group) <- c()
        length(group)
      }
    }
  })
  
  ### if length <= 6 -> plot
  output$group_regPlot <- renderPlot({
    group <- unlist(groups.long[which(names(groups.long) == input$groupFut)])
    names(group) <- c()
    group_reg(group = group)
  })
  
  ### if length > 6 -> Data.Table
  weeksFut <- reactive({
    if (input$quantityFut == 'familyFut') {
      if (input$groupFut != "Bitte waehlen") {
        list(helpText("Welche Produkten gehen in den nächsten x-Wochen aus?"),
             numericInput("weeksFut", "Wochen", value = 4))
      }
    }
  })
  output$weeksFut <- renderUI({
    if (input$quantityFut == 'familyFut')
      return(weeksFut())
  })
  output$group_regTab <- renderDataTable({
    group <- unlist(groups.long[which(names(groups.long) == input$groupFut)])
    names(group) <- c()
    group_reg(group, weeks = input$weeksFut, list = T)
  })
  
  
  ################################### current storage ##############################################
  output$currentStorage <- renderPlot({
    
    if (input$group != 'Bitte waehlen') {
      group <- unlist(groups.long[which(names(groups.long) == input$group)])
      names(group) <- c()
      currentStorage(group)
    }
  })
  
  output$currentStorageSum <- renderPlot({
    big.list <- lapply(groups.long, currentStorage, summary = TRUE)
    full <- lapply(big.list, '[[', 1)
    names(full) <- c()
    full <- unlist(full)
    empty <- lapply(big.list, '[[', 2)
    names(empty) <- c()
    empty <- unlist(empty)
    barplot(sort(full, decreasing = TRUE), 
          horiz = TRUE,
          las = 1,
          cex.axis = 0.8,
          cex.names = 0.8,
          xlab = "Warenbestand in Kilo")
  if (length(empty) != 0) { # usual usecase
    legend("topright", 
           legend = c("Derzeit vergriffen:", sort(empty, decreasing = TRUE)),
           pch = c(NA, rep(16, length(empty))))
    }
  })
  
})

####################################### shiny app executing ################################################
shinyApp(ui, server)
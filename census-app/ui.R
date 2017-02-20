library(shiny)
library(shinyjs)
library(lubridate)
library(data.table)
product.group <- list("Bitte wählen" = "Bitte waehlen",
                      "Zusammenfassung",
                      "Hülsenfrüchte" = "Huelsenfruechte", 
                      "Ölsaaten" = "Oelsaaten", 
                      "Gewürze" = "Gewuerze", 
                      "Putzequipment",
                      "Öl und Essig" = "Oel.Essig", 
                      "Getreideprodukte", 
                      "Getränke" = "Getraenke", 
                      "Aufstriche",
                      "Sonstiges")
data.pars <- c("from", "to")
data.list <- list("Von" = "from", "Bis" = "to")
graphic.pars <- c("type", "pch", "las", "lwd", "lty")
graphic.list <- list("Plot-Typ" = "type", "Punktart" = "pch", "Ausrichtung y-Achse" = "las", "Liniendicke" = "lwd", "Linientyp" = "lty")
scription.pars <- c("main_header", "xlab", "ylab")
scription.list <- list("Überschrift" = "main_header", "x-Achse" = "xlab", "y-Achse" = "ylab")
col.pars <- c("col_points", "col_reg", "col_conv", "col_20", "col_past")
col.list <- list("Punkte" = "col_points", "Zukunft-Mitte" = "col_reg", "Konvidenzintervalle" = "col_conv", "Farbe-4-Wochen" = "col_20", "Vergangenheit" = "col_past")
which.smoother <- c("smoother")
smoother.pars <- c("span", "degree")
advanced.pars <- c("nec.dates", "more.than", which.smoother, smoother.pars)
fun_reg.pars <- c(data.pars, graphic.pars, scription.pars, col.pars, advanced.pars)
fun_reg.list <- list("Zeitangaben" = "data.pars", 
                     "Graphische Parameter" = "graphic.pars",
                     "Beschriftung" = "scription.pars",
                     "Farben" = "col.pars",
                     "Für Fortgeschrittene" = "advanced.pars")

shinyUI(fluidPage(
  titlePanel("Kornkammer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("what", 
                   label = "Aktuell  oder Zukunft?",
                   choices = list("Aktueller Warenbestand" = 'present', 
                                  "Zukunftsprognose" = 'future')),
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
                                        selectInput("group",
                                                    "Produktgruppen",
                                                    choices = product.group))),
      conditionalPanel(condition = "input.what == 'present'",
                       selectInput("numbStock",
                                   "Nach Gruppe oder Lieferant?",
                                   choices = list("Bitte wählen" = 0,
                                                  "Produktgruppe" = 1,
                                                  "Lieferant" = 2)),
                       conditionalPanel(condition = "input.numbStock == 1",
                                        selectInput("group",
                                                    "Produktgruppen",
                                                    choices = product.group))),
      
      conditionalPanel(condition = "input.number != 0 | input.numbStock != 0",
                       checkboxInput("settings",
                                     label = "Erweiterte Einstellungen",
                                     value = FALSE),
                       conditionalPanel(condition = "input.settings == true",
                                        checkboxGroupInput("pars", label ="Parametergruppen",
                                                           choices = fun_reg.list),
                                        conditionalPanel(condition = "input.pars == 'data.pars'",
                                                         checkboxGroupInput("data", "Zeitangaben", data.list)),
                                        conditionalPanel(condition = "input.pars == 'graphic.pars'",
                                                         checkboxGroupInput("graphics", "Grafikeinstellungen", graphic.list)),
                                        conditionalPanel(condition = "input.pars == 'scription.pars'",
                                                         checkboxGroupInput('scription', "Beschriftung", scription.list)),
                                        conditionalPanel(condition = "input.pars == 'col.pars'",
                                                         checkboxGroupInput('cols', "Farben", col.list))
                                        )),
      conditionalPanel(condition = "input.data.includes('from')",
                       dateInput("from", "Von", value = as.character(Sys.Date() %m-% months(6)))),
      conditionalPanel(condition = "input.cols.includes('col_points')",
                       colourInput("col_points", "Farbe der Punkte", value = "lightgrey"))
      ),
      
    
    mainPanel(
      plotOutput("prodPlot")#,
      #verbatimTextOutput("class")
    )
  )
  
))
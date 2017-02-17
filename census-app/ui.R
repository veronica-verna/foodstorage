library(shiny)
library(lubridate)
library(data.table)

shinyUI(fluidPage(
  titlePanel("Kornkammer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Argument selecter:
      htmlOutput("ArgSelect"),
      # Argument field:
      htmlOutput("ArgText"),
      # Upload data:
      fileInput("kornumsatz", "Upload 'kornumsatz':",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
      
      selectInput("what", 
                  label = "Aktuell  oder Zukunft?",
                  choices = list("Bitte auswählen" = "Bitte auswaehlen", 
                                 "Aktueller Warenbestand", 
                                 "Zukunftsprognose"),
                  selected = "Bitte auswaehlen"),
      conditionalPanel(condition = "input.what == 'Zukunftsprognose'",
                       radioButtons("number",
                                    "Einzelnes Produkt oder eine Gruppe?",
                                    choices = list("Einzelnes Produkt" = 0,
                                                   "Produktgruppe (Familie)" = 1,
                                                   "Lieferanten" = 2))),
      # Which product?
      conditionalPanel(condition = "input.number == 0",
                       selectizeInput("product",
                                      "Produkt",
                                      choices = c("Bitte wählen" = "Bitte waehlen", levels(kornumsatz$Produkt)), 
                                      select = "Bitte waehlen",
                                      options = list(create = TRUE,
                                                     placeholder = lapply(levels(kornumsatz$Produkt), '['),
                                                     maxItems = 1))),
      
      conditionalPanel(condition = "input.number == 1",
                       selectInput("group",
                                   "Produktgruppen",
                                   choices = list("Biite auswählen" = "Bitte auswaehlen",
                                                  "Zusammenfassung",
                                                  "Hülsenfrüchte" = "Huelsenfruechte", 
                                                  "Ölsaaten" = "Oelsaaten", 
                                                  "Gewürze" = "Gewuerze", 
                                                  "Putzequipment",
                                                  "Öl und Essig" = "Oel.Essig", 
                                                  "Getreideprodukte", 
                                                  "Getränke" = "Getraenke", 
                                                  "Aufstriche",
                                                  "Sonstiges"),
                                   selected = "Bitte auswaehlen")),
      conditionalPanel(condition = "input.number != 'Bitte auswaehlen' | input.product != 'Bitte waehlen'",
                       uiOutput('future')
                       ),
      checkboxInput("settings",
                    label = "Grafikoptionen",
                    value = FALSE),
      conditionalPanel(condition = "input.settings == true",
                       checkboxGroupInput("optional", label ="",
                                          choices = formals("fun_reg")))
      
      
      ),
      
    
    mainPanel(
      plotOutput("prodPlot")
    )
  )
  
))
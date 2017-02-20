library(shiny)
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
                                     label = "Grafikoptionen",
                                     value = FALSE),
                       conditionalPanel(condition = "input.settings == true",
                                        checkboxGroupInput("optional", label ="",
                                                           choices = formals("fun_reg")[-c(1, length(formals(fun_reg)))] )))),
      
    
    mainPanel(
      plotOutput("prodPlot")
    )
  )
  
))
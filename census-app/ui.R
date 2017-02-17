library(shiny)

shinyUI(fluidPage(
  titlePanel("Kornkammer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("what", 
                  label = "Aktuell  oder Zukunft?",
                  choices = list("Bitte auswählen" = 1, "Aktueller Warenbestand" = 2,
                                 "Zukunftsprognose" = 3),
                  selected = 1),
      conditionalPanel(condition = "input.what == 2",
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
                                   selected = 1)),
      # Which product?
      conditionalPanel(condition = "input.what == 3",
                       selectizeInput("product",
                                      "Produkt",
                                      choices = levels(kornumsatz$Produkt),
                                      options = list(create = TRUE,
                                                     placeholder = unlist(startup.settings(kornumsatz, prod.list = TRUE))[1],
                                                     maxItems = 1))),
      checkboxInput("settings",
                    label = "Grafikoptionen",
                    value = FALSE),
      conditionalPanel(condition = "input.settings == true",
                       checkboxGroupInput("optional", label ="",
                                          choices = list("Von" = 1,
                                                         "Bis" = 2,
                                                         "span" = 3,
                                                         "Punktfarbe" = 4,
                                                         "Linienfarbe" = 5)))
      
      
      ),
      
    
    mainPanel(
      plotOutput("prodPlot")
    )
  )
  
))
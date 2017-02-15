library(shiny)

shinyUI(fluidPage(
  titlePanel("Kornkammer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins",
                  min = 1, max =50, value = 30),
      checkboxInput("settings",
                    label = "Graphische Einstellungen",
                    value = FALSE),
      conditionalPanel(condition = "input.settings == true",
                       checkboxGroupInput("optional", label ="",
                                          choices = list("Von" = 1,
                                                         "Bis" = 2,
                                                         "span" = 3,
                                                         "Punktfarbe" = 4,
                                                         "Linienfarbe" = 4))),
      selectInput("what",
                  label = "Aktuell oder Zukunft?",
                  choices = list("Bitte wählen" = 1,
                                 "Aktueller Warenbestand" = 2,
                                 "Zukunftsprognose" = 3),
                  selected = 1)),
      
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
  
))
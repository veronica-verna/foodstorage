library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Warenbestand der Kornkammer"),
  
  fluidRow(
    column(3,
           textInput("text", label = h4("Gleiche Produkte - verschiedene Namen"),
                     value = "Produktnamen..."),
           checkboxGroupInput("checkGroup",
                              label = h4("Produkte zur Auswahl")))
  )
  
  #sidebarLayout(
  # sidebarPanel( "Einstellungen"),
  # mainPanel("Und hier das Ergebnis")
  #)
))
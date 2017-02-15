library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # read the data
  kornumsatz <- read.csv("kornumsatz.csv", sep =";")
  kornumsatz <- startup.settings(kornumsatz)
  
  output$grund <- renderText({
    paste("you have selected", input$var)
  })
  # 
  
})

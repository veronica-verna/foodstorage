# read the data
#kornumsatz <- read.csv("kornumsatz.csv", sep =";")
#kornumsatz <- startup.settings(kornumsatz)

shinyServer(function(input, output){
  
  
  
  output$distPlot  <- renderPlot({
    x <- faithful[,2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins)
  })
})
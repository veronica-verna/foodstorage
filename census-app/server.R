
shinyServer(function(input, output){
    
  
  output$prodPlot  <- renderPlot({
    fun_reg(product = input$product, main_header = input$product)
    #currentStorage(group = input$group)
    #x <- faithful[,2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #hist(x, breaks = bins)
  })
  #output$textPlot <- renderText(input$group)
})
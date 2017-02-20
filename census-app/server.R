
shinyServer(function(input, output){
  
  output$prodPlot  <- renderPlot({
    if (input$product != 'Bitte waehlen') {
      fun_reg(product = input$product, main_header = input$product)
    }
  })
  
})
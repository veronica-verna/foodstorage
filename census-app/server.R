
shinyServer(function(input, output){
  
  output$prodPlot  <- renderPlot({
    if (input$product != 'Bitte waehlen' && input$settings != TRUE) {
      fun_reg(product = input$product, main_header = input$product)
    }
    if (input$settings == TRUE && 'from' %in% input$graphical) {
      fun_reg(product = input$product, main_header = input$product, from = as.character(input$from))
    }
    if (input$settings == TRUE && 'col_points' %in% input$graphical) {
      fun_reg(product = input$product, main_header = input$product, col_points = input$col_points)
    }
  })
  output$class <- renderText({
    if (input$settings == TRUE && 'from' %in% input$graphical) {
      #c(class(input$from), as.character(input$from), as.character(as.Date(input$from, origin = "1970-01-01")))
    }
  })
})
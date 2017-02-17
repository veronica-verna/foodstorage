
shinyServer(function(input, output){
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(read.csv2)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  ### Argument names German:
  ArgGerman <- reactive({
    ger <- c("Überschrift", "Feldtrenner", "quote", "Dezimaltrenner", "fill", "comment.char")
    return(ger)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    selectInput("arg","Argumente:", 
                choices = c("Standard" = "default", ArgNames()))
  })
  
  ## Arg text field:
  output$ArgText <- renderUI({
    Defaults <- formals(read.csv2)
    arg <- names(formals(read.csv2))[-c(1, length(Defaults)-1, length(Defaults))]
    def <- Defaults[-c(1, length(Defaults)-1, length(Defaults))]
    
    ## which paramter do you wanna change?
    if (input$arg == "header") {
      radioButtons("head",
                   "Überschrift",
                   choices = list("Ja" = 0, "Nein" = 1),
                   selected = 0)
    } else {
      if (input$arg == "sep") {
        radioButtons("seperator", 
                     "Textfeldtrenner",
                     choices = list(";" = 0, "," = 1, "." = 2, " " = 3),
                     selected = 0)
      } else {
        if (input$arg == "dec") {
          radioButtons("decimal",
                      "Dezimaltrenner",
                      choices = list("." = 0, "," = 1))
        } else {
          if (input$arg == "default") 
            actionButton("read", "'Kornumsatz' einlesen")
        }
      }
    }
  
    
    
      
    
    #if (input$arg == "quote")
      
    
    
    
    
  })
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^","read.csv","__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    return(Dataset)
  })
  
  output$prodPlot  <- renderPlot({
    fun_reg(product = input$product, main_header = input$product)
    #currentStorage(group = input$group)
    #x <- faithful[,2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #hist(x, breaks = bins)
  })
  #output$textPlot <- renderText(input$group)
})
###################################################################################################
###################### read kornumsatz ############################################################
###################################################################################################
data("kornumsatz_demo", package = "foodstorage")
data("starting_csv", package = "foodstorage")
# '<<-' important because kornumsatz must be in globalenv() that it can be found by functions in server UI
kornumsatz <<- kornumsatz_demo
kornumsatz$Produkt <<- as.character(kornumsatz$Produkt)
kornumsatz <<- foodstorage::startup.settings(kornumsatz, importPRODUCTS = starting_csv)
kornumsatz$Produkt <<- as.factor(kornumsatz$Produkt)

createShinyList <- function(what = "dt", check = FALSE) {
  if (what == "dt") {
    if (check == TRUE) return("plot1")
    plot_output_list <- list(dataTableOutput("plot1"))
  } 
  if (what == "plot") {
    if (check == TRUE) return("plot2")
    plot_output_list <- list(plotOutput("plot2"))
  }
  
  return(plot_output_list)
}

###################################################################################################
########################################### Shiny UI ##############################################
###################################################################################################
ui <- shinyUI(navbarPage("Kornkammer", id = "tabs", selected=1,
                         
                ################################ Warenbestand #####################################
                tabPanel("Warenbestand", value = 1, 
                         ########################### Header #######################################
                         h2("Du willst wissen, was in der Kornkammer gerade vorrätig ist?")
                ),
                
                fluidRow(
                  selectInput(
                    "choice",
                    "Auswahl",
                    choices = list("tabellarisch" = "dt",
                                   "grafisch" = "plot")
                  ),
                  actionButton("go", "Warenbestand anzeigen")
                ),
                br(),
                
                ###################################################################################
                ############################# Main Panel ##########################################
                ###################################################################################
                
                fluidRow(
                  uiOutput("plots")
                )
    
  ))


###################################################################################################
####################################### Shiny Server ##############################################
###################################################################################################


server <- shinyServer(function(input, output, session){
  
  output$plots <- renderUI({
    # Create a list of `plotOutput` objects (depending on current())
    plot_output_list <- createShinyList(input$choice)
    # Place the plot output inside a shiny `tagList()`
    do.call(tagList, plot_output_list)
  })
  
  # Every time a plot changes (button is clicked), re-generate the render functions for all the plots
  observeEvent(
    input$go,
    label = "renderingPlots", {
      plotname <- createShinyList(input$choice, check = T)
      
      if (plotname == "plot1") {
        output[[plotname]] <- renderDataTable({
          foodstorage::currentStorage(levels(kornumsatz$Produkt), rawlist = TRUE)
        })
      }
      
      if (plotname == "plot2") {
        output[[plotname]] <- renderPlot({
          foodstorage::currentStorage(levels(kornumsatz$Produkt))
        })
      }
    }
  )
  
})

####################################### shiny app executing ################################################
shinyApp(ui, server, options = list(port = 1234))
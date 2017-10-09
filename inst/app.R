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

########################## Level 1 = Input 1: Group or product ####################################
level1st <- list("Zusammenfassung" = 'summary',
                 "Einzelnes Produkt" = 'ONEprod',
                 "Produktgruppe (Familie)" = 'family',
                 "Lieferanten" = 'producer')

########################### Level 2 = Input 2: product groups #####################################
product.group <- as.character(unique(starting_csv$Produktgruppe))
deliverers <- as.character(unique(starting_csv$Lieferant))
# deliverers2 <- as.character(unique(starting_csv$Lieferant2))
# deliverers2 <- deliverers2[which(deliverers2 != "")]
level2nd <- list("ONEprod" = as.character(levels(kornumsatz$Produkt)), # 'Einzelnes Produkt' in UI
                 "family" = product.group, # 'Produktgruppe (Familie)' in UI
                 "producer" = deliverers) #, 'Lieferanten' in UI
                 # "producer2" = deliverers2)

prodBYprod <- list()
for (i in 1:length(product.group)) {
  prodBYprod[[i]] <- as.character(unique(starting_csv[starting_csv$Produktgruppe == product.group[i],]$Produkte_Zusammenfassung))
  names(prodBYprod)[i] <- product.group[i]
}
prodBYdel1 <- list()
for (i in 1:length(deliverers)) {
  prodBYdel1[[i]] <- as.character(unique(starting_csv[starting_csv$Lieferant == deliverers[i],]$Produkte_Zusammenfassung))
  names(prodBYdel1)[i] <- deliverers[i]
}
# prodBYdel2 <- list()
# for (i in 1:length(deliverers2)) {
#   prodBYdel2[[i]] <- as.character(unique(starting_csv[starting_csv$Lieferant2 == deliverers2[i],]$Produkte_Zusammenfassung))
#   names(prodBYdel2)[i] <- deliverers2[i]
# }

###################################################################################################
################### function for creating html output for renderUI ################################
createShinyList <- function(tabs, quantity, prod, from, to, before, check = FALSE, plot = FALSE) {
  ############################# present plotting ##################################################
  if (tabs == 1 && quantity == "summary") {
    if (check == TRUE) return("plot1")
    if (plot == TRUE) {
      foodstorage::currentStorage(levels(kornumsatz$Produkt))
    } else plot_output_list <- list(plotOutput("plot1", height = 1000))
    # otherwise create a html tag for renderUI
    
  }
  
  if (tabs == 1 && quantity == "ONEprod") {
    if (check == TRUE) return("plot2")
    if (plot == TRUE) {
      suppressWarnings(foodstorage::plotStorage(foodstorage::prepare(prod, what.plotting = "Warenbestand", myPlot = TRUE, from = from, to = to)))
    } else plot_output_list <- list(plotOutput("plot2"))
    # otherwise create a html tag for renderUI
    
  }
  
  if (tabs == 1 && quantity == "family") {
    if (check == TRUE) return("plot3")
    if (plot == TRUE) {
      foodstorage::currentStorage(unlist(prodBYprod[which(names(prodBYprod) == prod)], use.names = F))
    } else plot_output_list <- list(plotOutput("plot3"))
    # otherwise create a html tag for renderUI
    
  }
  
  if (tabs == 1 && quantity == "producer") {
    if (check == TRUE) return("plot4")
    if (plot == TRUE) {
      foodstorage::currentStorage(unlist(prodBYdel1[which(names(prodBYdel1) == prod)], use.names = F))
    } else plot_output_list <- list(plotOutput("plot4"))
    # otherwise create a html tag for renderUI
    
  }
  
  ################################### plotting future #############################################
  
  if (tabs == 2 && quantity == "summary") {
    if (check == TRUE) return("plot5")
    if (plot == TRUE) {
      return(foodstorage::prognosEs(as.character(levels(kornumsatz$Produkt)), before = before, list = T))
    } else plot_output_list <- list(DT::dataTableOutput("plot5"))
  }
  
  if (tabs == 2 && quantity == "ONEprod") {
    if (check == TRUE) return("plot6")
    if (plot == TRUE) {
      foodstorage::prognosIs(prod, main_header = prod, from = from, to = to)
    } else plot_output_list <- list(plotOutput("plot6"))
  }
  
  if (tabs == 2 && quantity == "family") {
    if (check == TRUE) return("plot7")
    if (plot == TRUE) {
      group <- unlist(prodBYprod[which(names(prodBYprod) == prod)], use.names = F)
      return(foodstorage::prognosEs(as.character(group), before = before, list = T))
    } else plot_output_list <- list(DT::dataTableOutput("plot7"))
  }
  
  if (tabs == 2 && quantity == "producer") {
    if (check == TRUE) return("plot8")
    if (plot == TRUE) {
      group <- unlist(prodBYdel1[which(names(prodBYdel1) == prod)], use.names = F)
      return(foodstorage::prognosEs(as.character(group), before = before, list = T))
    } else plot_output_list <- list(DT::dataTableOutput("plot8"))
  }
  
  if (check == FALSE && plot == FALSE) return(plot_output_list)
}

###################################################################################################
########################################### Shiny UI ##############################################
###################################################################################################
ui <- shinyUI(navbarPage("Kornkammer", id = "tabs", selected=1,
                         ##################### Theme settings #####################################
                         #theme = shinytheme("united"),
                         # busy-button
                         tags$head(tags$style(type = "text/css", "#loadmessage {
                                                                  position: fixed;
                                                                  top: 35%;
                                                                  left: 0;
                                                                  width: 100%;
                                                                  padding: 5px 0px 5px 0px;
                                                                  font-weight: bold;
                                                                  font-size: 100%;
                                                                  color: #000000;
                                                                  background-color: #cc6600;
                                                                  opacity: 0.7;
                                                                  filter: alpha(opacity=60); /* For IE8 and earlier */
                                                                  z-index: 105;
                                                                  box-shadow: inset 0 1px 2px rgba(0, 0, 0, 0.1);
                                                                 }
                                                              "
                                            )
                         ),
                         
                ################################ Warenbestand #####################################
                tabPanel("Warenbestand", value = 1, 
                         ########################### Header #######################################
                         h2("Du willst wissen, was in der Kornkammer gerade vorrätig ist?"),
                         
                        fluidRow(
                          conditionalPanel(condition="$('html').hasClass('shiny-busy') && input.tabs==1 
                                           && input.quantity == 'summary'",
                                           tags$div(h1("Bin am nachschauen..."), id="loadmessage"))
                        )
                ),
                
                ################################# Zukunftsprognose ################################
                tabPanel("Zukunftsprognose", value = 2,
                         
                         ########################### Header #######################################
                         h2("Du möchtest wissen, was als nächstes nachbestellt werden sollte?"),
                         h5(em("Für manche Produkte kann aus verschiedenen Gründen keine Zukunftsprognose gemacht werden. Diese sind gekennzeichnet mit 'kPm' (keine Prognose möglich). In diesen Fällen schaut euch einfach die Warenbestände an.")),
                         br(),
                         fluidRow(
                           conditionalPanel(condition="$('html').hasClass('shiny-busy') && input.tabs==2 
                                            && input.quantity != 'ONEprod'",
                                            tags$div(h1("Puh - Rechnen ist anstrengend..."), id="loadmessage"))
                         )
                         
                         ),
                
                ########################### Basic Settings ########################################
                fluidRow(
                  column(4,
                         selectInput("quantity",
                                     "Einzelnes Produkt oder Gruppe?",
                                     choices = level1st)
                  ),
                  column(4,
                    # Which product?
                    conditionalPanel(
                      condition = "input.quantity != 'summary'",
                      selectInput("product","Produkt",
                                  choices = c("Bitte wählen" = "", level2nd$ONEprod)
                                  #options = list(placeholder = lapply(levels(kornumsatz$Produkt), '['), maxItems = 1))
                      )
                    ),
                    conditionalPanel(
                      condition = "input.tabs == 1 && input.quantity == 'ONEprod' || input.tabs == 2",
                      checkboxInput("options", "Erweiterte Optionen")
                    )
                  ),
                  column(4,
                    # more options
                    conditionalPanel(
                      condition = "input.options == true && input.tabs == 1 && input.quantity == 'ONEprod'||
                      input.options == true && input.tabs == 2 && input.quantity == 'ONEprod'",
                      dateRangeInput("range", "Zeitraum", start = Sys.Date() - months(6), end = Sys.Date(), language = "de")
                    ),
                    conditionalPanel(
                      condition = "input.options == true && input.tabs == 2 && input.quantity != 'ONEprod'",
                      dateInput("filter", "Geht aus am", value = Sys.Date() + weeks(4), min = Sys.Date() + weeks(1),
                                language = "de"),
                      helpText("Es werden nur noch Produkte angezeigt, die bis zum angegebenen Datum ausgehen werden.")
                    )
                  )
                ),
                
                fluidRow(
                  conditionalPanel(condition = "input.quantity == 'summary' |
                                                input.product != 'Bitte wählen'",
                                   actionButton("go",
                                                "Nachschlagen"))
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
  ############################### Updating Input Settings #########################################
  observeEvent(input$quantity, label = "Update2ndLevel", {
    # Update based on the quantity change event
    updateSelectInput(session, "product", 
                      label = names(level1st[which(level1st == input$quantity)]),
                      choices = c("Bitte wählen" = "",
                                  unlist(level2nd[which(names(level2nd) == input$quantity)], use.names = FALSE)))
  })
  
  ############################# reset inputs when tabs are changed ################################
  observeEvent(input$tabs, label = "resetQuantity", {
    # Reset quantity input when tab is changed
    updateSelectInput(session, "quantity", "Einzelnes Produkt oder Gruppe?",
                      choices = level1st)
    # text of action button depends on input$tabs
    if (input$tabs == 1) {
      updateActionButton(session, "go", "Nachschlagen")
    } else {
      updateActionButton(session, "go", "Berechnen")
    }
  })
  
  #################################################################################################
  ########################### what shall be plotted ###############################################
  current <- reactiveValues(
    tabs = 1,
    quantity = "summary",
    prod = "Allesreiniger",
    range = as.Date(c("2017-03-05", "2017-09-05")),
    filter = as.Date("2017-10-03")
  )
  
  observeEvent(input$go,label = "UpdatingCurrent", priority = 1, {
    current$tabs <- input$tabs
    current$quantity <- input$quantity
    current$prod <- input$product
    current$range <- as.character(input$range)
    current$filter <- as.character(input$filter)
    # for debugging:
    # print(c(current$tabs, current$quantity, current$prod, current$range, current$filter)) 
  })

  #################################################################################################
  ########################### generate outputs ####################################################
  #################################################################################################
  
  output$plots <- renderUI({
    # Create a list of `plotOutput` objects (depending on current())
    plot_output_list <- createShinyList(current$tabs, current$quantity)
    # Place the plot output inside a shiny `tagList()`
    do.call(tagList, plot_output_list)
  })
  
  # Every time a plot changes (button is clicked), re-generate the render functions for all the plots
  observeEvent(input$go, 
               label = "renderingOutput[[plotname]]",{ # label only for debugging
    
      plotname <- createShinyList(current$tabs, current$quantity, check = TRUE)
      print(plotname) # for debugging
      print(current$prod)
      # summary of 'Warenbestand'
      if (plotname == "plot1") {
        output[[plotname]] <- renderPlot({
          createShinyList(current$tabs, current$quantity, plot = TRUE)
        })
      }
      
      # ONEprod, family, deliverer of 'Warenbestand' & ONEprod of 'Zukunftsprognose'
      if (plotname %in% c("plot2", "plot3", "plot4", "plot6")) {
        output[[plotname]] <- renderPlot({
          createShinyList(current$tabs, current$quantity, current$prod, current$range[1], current$range[2], plot = TRUE)
        })
      }
      
      # summary, family and deliverer of 'Zukunftsprognose'
      if (plotname %in% c("plot5", "plot7", "plot8")) {
        output[[plotname]] <- DT::renderDataTable({
          input$go # I don't know why but this command is necessary - otherwise dt doesn't appear
          datatable(createShinyList(current$tabs, current$quantity, current$prod, before = current$filter, plot = TRUE), 
                    options = list(pageLength = 50, language = list(search = "Filter:"))) %>%
            formatDate("Datum")
        })
      }
  })
  
})

####################################### shiny app executing ################################################
shinyApp(ui, server, options = list(port = 1234))
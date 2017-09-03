###################################################################################################
###################### read kornumsatz ############################################################
###################################################################################################
data("kornumsatz_demo", package = "foodstorage")
data("starting_csv", package = "foodstorage")
# '<<-' important because kornumsatz must be in globalenv() that it can be found by functions in server UI
kornumsatz <<- kornumsatz_demo
kornumsatz$Produkt <<- as.character(kornumsatz$Produkt)
kornumsatz <<- startup.settings(kornumsatz, importPRODUCTS = starting_csv)
kornumsatz$Produkt <<- as.factor(kornumsatz$Produkt)

#################################### Level 1: Group or product ####################################
level1st <- list("Zusammenfassung" = 'summary',
                 "Einzelnes Produkt" = 'ONEprod',
                 "Produktgruppe (Familie)" = 'family',
                 "Lieferanten" = 'producer')

##################################### Level 2: product groups #####################################
product.group <- as.character(unique(starting_csv$Produktgruppe))
deliverers <- as.character(unique(starting_csv$Lieferant))
deliverers2 <- as.character(unique(starting_csv$Lieferant2))
deliverers2 <- deliverers2[which(deliverers2 != "")]
level2nd <- list("ONEprod" = as.character(levels(kornumsatz$Produkt)),
                 "family" = product.group,
                 "producer" = deliverers,
                 "producer2" = deliverers2)

prodBYprod <- list()
for (i in length(product.group)) {
  prodBYprod[[i]] <- as.character(unique(starting_csv[starting_csv$Produktgruppe == product.group[i],]$Produkte_Zusammenfassung))
  names(prodBYprod)[i] <- product.group[i]
}
prodBYdel1 <- list()
for (i in 1:length(deliverers)) {
  prodBYdel1[[i]] <- as.character(unique(starting_csv[starting_csv$Lieferant == deliverers[i],]$Produkte_Zusammenfassung))
  names(prodBYdel1)[i] <- deliverers[i]
}
prodBYdel2 <- list()
for (i in 1:length(deliverers2)) {
  prodBYdel2[[i]] <- as.character(unique(starting_csv[starting_csv$Lieferant2 == deliverers2[i],]$Produkte_Zusammenfassung))
  names(prodBYdel2)[i] <- deliverers2[i]
}

###################################################################################################
################### function for creating html output for renderUI ################################
createShinyList <- function(tabs, quantity, prod, weeks, check = FALSE, plot = FALSE) {
  ############################# present plotting ##################################################
  if (tabs == 1 && quantity == "summary") {
    if (check == TRUE) return("plot1")
    if (plot == TRUE) {
      big.list <- lapply(prodBYprod, currentStorage, summary = TRUE)
      full <- lapply(big.list, '[[', 1)
      names(full) <- c()
      full <- unlist(full)
      empty <- lapply(big.list, '[[', 2)
      names(empty) <- c()
      empty <- unlist(empty)
      barplot(sort(full, decreasing = TRUE), 
              horiz = TRUE,
              las = 1,
              cex.axis = 0.8,
              cex.names = 0.8,
              xlab = "Warenbestand in Kilo")
      if (length(empty) != 0) { # usual usecase
        legend("topright", 
               legend = c("Derzeit vergriffen:", sort(empty, decreasing = TRUE)),
               pch = c(NA, rep(16, length(empty))))
      }
    } else plot_output_list <- list(plotOutput("plot1", height = 1000))
    # otherwise create a html tag for renderUI
    
  }
  
  if (tabs == 1 && quantity == "ONEprod") {
    if (check == TRUE) return("plot2")
    if (plot == TRUE) {
      suppressWarnings(plotStorage(prepare(prod, what.plotting = "Warenbestand", myPlot = TRUE)))
    } else plot_output_list <- list(plotOutput("plot2"))
    # otherwise create a html tag for renderUI
    
  }
  
  if (tabs == 1 && quantity == "family") {
    if (check == TRUE) return("plot3")
    if (plot == TRUE) {
      currentStorage(unlist(prodBYprod[which(names(prodBYprod) == prod)], use.names = F))
    } else plot_output_list <- list(plotOutput("plot3"))
    # otherwise create a html tag for renderUI
    
  }
  
  if (tabs == 1 && quantity == "producer") {
    if (check == TRUE) return("plot4")
    if (plot == TRUE) {
      currentStorage(unlist(prodBYdel1[which(names(prodBYdel1) == prod)], use.names = F))
    } else plot_output_list <- list(plotOutput("plot4"))
    # otherwise create a html tag for renderUI
    
  }
  
  ################################### plotting future #############################################
  
  if (tabs == 2 && quantity == "summary") {
    if (check == TRUE) return("plot5")
    if (plot == TRUE) {
      prognosEs(levels(kornumsatz$Produkt), weeks = weeks, list = T)
    } else plot_output_list <- list(dataTableOutput("plot5"))
  }
  
  if (tabs == 2 && quantity == "ONEprod") {
    if (check == TRUE) return("plot6")
    if (plot == TRUE) {
      prognosIs(product = prod, main_header = prod)
    } else plot_output_list <- list(plotOutput("plot6"))
  }
  
  if (tabs == 2 && quantity == "family") {
    if (check == TRUE) return("plot7")
    if (plot == TRUE) {
      group <- unlist(prodBYprod[which(names(prodBYprod) == prod)], use.names = F)
      prognosEs(group, weeks = weeks, list = T)
    } else plot_output_list <- list(dataTableOutput("plot7"))
  }
  
  if (tabs == 2 && quantity == "producer") {
    if (check == TRUE) return("plot8")
    if (plot == TRUE) {
      group <- unlist(prodBYdel1[which(names(prodBYdel1) == prod)], use.names = F)
      prognosEs(group, weeks = weeks, list = T)
    } else plot_output_list <- list(dataTableOutput("plot8"))
  }
  
  if (check == FALSE && plot == FALSE) return(plot_output_list)
}

###################################################################################################
########################################### Shiny UI ##############################################
###################################################################################################
ui <- shinyUI(navbarPage("Kornkammer", id = "tabs", selected=1,
                         ##################### Theme settings #####################################
                         theme = shinytheme("united"),
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
                                            && input.quantityFut == 'summary'",
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
                         conditionalPanel(condition = "input.quantity != 'summary'",
                                          selectInput("product",
                                                      "Produkt",
                                                      choices = c("Bitte wählen" = "", level2nd$ONEprod)
                                                      #options = list(placeholder = lapply(levels(kornumsatz$Produkt), '['), maxItems = 1))
                                          )
                         ),
                  column(4,
                         conditionalPanel(condition = "input.quantity != 'ONEprod' && input.tabs == 2",
                                          numericInput("weeks",
                                                       "Wochen", value = 4, min = 1, max = 30))
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
  observeEvent(input$quantity, label = "Update2ndLevel", suspended = TRUE, {
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
    weeks = 4
  )
  
  observeEvent(input$go, suspended = TRUE,label = "UpdatingCurrent", {
    current$tabs <- input$tabs
    current$quantity <- input$quantity
    current$prod <- input$product
    if (exists(input$weeks)) current$weeks <- input$weeks
  })
  
  #################################################################################################
  ########################### generate outputs ####################################################
  #################################################################################################
  
  output$plots <- renderUI({
    # Create a list of `plotOutput` objects (depending on current())
    plot_output_list <- createShinyList(tabs = current$tabs, quantity = current$quantity)
    print(plot_output_list)
    # Place the plot output inside a shiny `tagList()`
    do.call(tagList, plot_output_list)
    print(do.call(tagList, plot_output_list))
  })
  
  # Every time a plot changes (button is clicked), re-generate the render functions for all the plots
  observeEvent(c(current$tabs, current$quantity, current$prod, current$weeks), label = "renderingOutput[[plotname]]",{
    #local({
      plotname <- createShinyList(current$tabs, current$quantity, check = TRUE)
      print(plotname)
      if (plotname == "plot1") {
        output[[plotname]] <- renderPlot({
          createShinyList(current$tabs, current$quantity, plot = TRUE)
        })
      }
      
      if (plotname %in% c("plot2", "plot3", "plot4", "plot6")) {
        print(exists(output[[plotname]]))
        output[[plotname]] <- renderPlot({
          print(current)
          createShinyList(current$tabs, current$quantity, current$prod, plot = TRUE)
        })
      }
      
      if (plotname %in% c("plot5", "plot7", "plot8")) {
        output[[plotname]] <- DT::renderDataTable({
          createShinyList(current$tabs, current$quantity, current$prod, current$weeks, plot = TRUE)
        })
      }
    #})
  })
  
})

####################################### shiny app executing ################################################
shinyApp(ui, server, options = list(port = 1234))
######################################################################
###################### read kornumsatz ###############################
######################################################################
data("kornumsatz_demo", package = "foodstorage")
data("starting_csv", package = "foodstorage")
print(str(kornumsatz_demo))
kornumsatz <- kornumsatz_demo
kornumsatz$Produkt <- as.character(kornumsatz$Produkt)
kornumsatz <- startup.settings(kornumsatz, importPRODUCTS = starting_csv)
kornumsatz$Produkt <- as.factor(kornumsatz$Produkt)
print(str(kornumsatz))
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
                                                                  text-align: center;
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
                  conditionalPanel(condition = "input.tabs == 1 && input.quantity == 'summary'",
                                   plotOutput("overview")),
                  conditionalPanel(condition = "input.tabs == 1 && input.quantity != 'summary' |
                                                input.tabs == 2 && input.quantity == 'ONEprod'",
                                   plotOutput("curves")),
                  conditionalPanel(condition = "input.tabs == 2 && input.quantity != 'ONEprod'",
                                   DT::dataTableOutput("tables"))
                )
    
  ))


###################################################################################################
####################################### Shiny Server ##############################################
###################################################################################################


server <- shinyServer(function(input, output, session){
  ############################### Updating Input Settings #########################################
  observeEvent(input$quantity, {
    # Update based on the quantity change event
    updateSelectInput(session, "product", 
                      label = names(level1st[which(level1st == input$quantity)]),
                      choices = c("Bitte wählen" = "",
                                  unlist(level2nd[which(names(level2nd) == input$quantity)], use.names = FALSE)))
  })
  
  ############################# reset inputs when tabs are changed ################################
  observeEvent(input$tabs, {
    # Reset quantity input when tab is changed
    updateSelectInput(session, "quantity", "Einzelnes Produkt oder Gruppe?",
                      choices = level1st)
  })
  
  ############################# text of action button depends on input$tabs #######################
  observeEvent(input$tabs, {
    if (input$tabs == 1) {
      updateActionButton(session, "go", "Nachschlagen")
    } else {
      updateActionButton(session, "go", "Berechnen")
    }
  })
  
  #################################################################################################
  ########################### generate outputs ####################################################
  #################################################################################################
  
  observeEvent(input$go, {
    ################################### present summary ###########################################
    output$overview <- renderPlot({
      if (input$tabs == 1 && input$quantity == "summary") {
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
      }
    }, height = 1000)
    
    #################### rest of present & future ONEprod #########################################
    output$curves <- renderPlot({
      if (input$tabs == 1 && input$quantity == "ONEprod") {
        suppressWarnings(plotStorage(prepare(input$product, what.plotting = "Warenbestand", myPlot = TRUE)))
      }
      if (input$tabs == 1 && input$quantity == "family") {
        currentStorage(unlist(prodBYprod[which(names(prodBYprod) == input$product)], use.names = F))
      }
      if (input$tabs == 1 && input$quantity == "producer") {
        currentStorage(unlist(prodBYdel1[which(names(prodBYdel1) == input$product)], use.names = F))
      }
      if (input$tabs == 2 && input$quantity == 'ONEprod') {
        prognosIs(product = input$product, main_header = input$product)
      }
    })
    
    ################################ rest of future ###############################################
    output$tables <- DT::renderDataTable({
      if (input$tabs == 2 && input$quantity == 'summary') {
        prognosEs(levels(kornumsatz$Produkt), weeks = input$weeks, list = T)
      }
      if (input$tabs == 2 && input$quantity == 'family') {
        group <- unlist(prodBYprod[which(names(prodBYprod) == input$product)], use.names = F)
        prognosEs(group, weeks = input$weeks, list = T)
      }
      if (input$tabs == 2 && input$quantity == 'producer') {
        group <- unlist(prodBYdel1[which(names(prodBYdel1) == input$product)], use.names = F)
        prognosEs(group, weeks = input$weeks, list = T)
      }
    })
  })
  
  ##########################################################################################
  ############################# rendering outputs ##########################################
  ##########################################################################################
  
  
  
})

####################################### shiny app executing ################################################
shinyApp(ui, server, options = list(port = 1234))
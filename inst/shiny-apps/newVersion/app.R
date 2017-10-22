###################################################################################################
###################### read kornumsatz ############################################################
###################################################################################################
path <- "/home/simon/Documents/Studium/Bachelor-Arbeit/R-paket/foodstorage"
# '<<-' important because kornumsatz must be in globalenv() that it can be found by functions in server UI
starting_csv <- read.csv(paste0(path, "/data/starting_csv.csv"), sep = ";")
kornumsatz <- read.csv(paste0(path, "/data/kornumsatz_demo.csv"), sep = ";")
kornumsatz <- foodstorage::startup.settings(kornumsatz, importPRODUCTS = starting_csv)

# check new data
data("kornumsatz_new")
addStartingCSV <<- equalise(oldData = kornumsatz_demo,
                            newData = kornumsatz_new,
                            startingCSV = starting_csv)


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
ui <- shinyUI(
  navbarPage(
    "Kornkammer", 
    id = "tabs", 
    selected=1,
    #################################### Warenbestand #############################################
    tabPanel(
      "Warenbestand", 
      value = 1, 
      # Header
      h2("Du willst wissen, was in der Kornkammer gerade vorrätig ist?"),
      # select input ...
      fluidRow(
        selectInput(
          "choice",
          "Auswahl",
          choices = list("tabellarisch" = "dt",
                         "grafisch" = "plot")
        )
      ),
      br(),
      fluidRow(
        column(2), # just for a better look
        column(4,
               actionButton("go1", "Nachschlagen")
        )
      ),
      # Main Panel 
      fluidRow(
        uiOutput("plots")
      )
    ),
    
    ############################# new data ########################################################
    tabPanel(
      "Neuen Datensatz abgleichen", 
      value = 2, 
      icon = icon("upload"),
      # Header 
      h2("Hier kannst du einen neuen Datensatz auf Konsistenz überprüfen"),
      h5(em("Falls es seit dem letzten Datensatz neue Produkte gegeben hat, wirst du hier durch eine Benutzeroberfläche geleitet, mittels derer du die neuen Produkte einer Produktgruppe und einem Lieferanten zuordnen kannst.")),
      br(),
      # first row is for name of product in the FoodCoApp and how the product shall be called in future
      fluidRow(
        column(
          4,
          selectizeInput(
            "newproducts", "Produktname in der App",
            choices = c("Bitte wählen" = "",
                        addStartingCSV)
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != ''",
            selectizeInput(
              "prodsummary", "Unter folgendem Namem zusammengefasst",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Produkte_Zusammenfassung),
                          "neues Produkt")
            ),
            helpText("Wähle 'neues Produkt' wenn wir es wirklich noch nie in der KoKa hatten und nicht in der Liste zu finden ist.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary == 'neues Produkt'",
            actionButton("go_prod", "Neues Produkt eintragen", icon = icon("send"))
          )
        )
      ),
      # 2nd row is for deliverers
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && input.prodsummary != 'neues Produkt'",
            selectizeInput(
              "deliverer", "Lieferant Nr. 1",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Lieferant),
                          "neuer Lieferant")
            ),
            helpText("Wähle 'neuer Lieferant', um einen neuen Lieferanten hinzuzufügen, bei dem zukünftig dieses Produkt bestellt werden soll.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && input.prodsummary != 'neues Produkt'",
            selectizeInput(
              "deliverer2", "Lieferant Nr. 2 (optional)",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Lieferant),
                          "neuer Lieferant")
            ),
            helpText("Wähle nichts aus, wenn es keinen zweiten, alternativen Lieferanten für dieses Produkt gibt oder füge einen neuen zweiten Lieferanten für dieses Produkt hinzu.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.deliverer == 'neuer Lieferant' || input.deliverer2 == 'neuer Lieferant'",
            actionButton("go_deliv", "neue(n) Lieferanten eintragen", icon = icon("send"))
          )
        )
      ),
      # 3rd row is for product group
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && input.prodsummary != 'neues Produkt'",
            selectizeInput(
              "prodgroup", "Produktgruppe",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Produktgruppe),
                          "neue Produktgruppe")
            ),
            helpText("Wähle 'neue Produktgruppe', um eine neue Produktgruppe hinzuzufügen.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.prodgroup == 'neue Produktgruppe'",
            actionButton("go_group", "neue Produktgruppe hinzufügen", icon = icon("send"))
          )
        )
      ),
      # 4th row is for bulk size
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && input.prodsummary != 'neues Produkt'",
            selectInput(
              "bulksize", "Verpackungseinheit",
              choices = c("Bitte wählen" = "",
                          sort(unique(starting_csv$Verpackungseinheit)),
                          "neue VPE")
            ),
            helpText("Wähle 'neue VPE', um eine neue Größe einzugeben.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.bulksize == 'neue VPE'",
            actionButton("go_bulk", "neue VPE eintragen", icon = icon("send"))
          )
        )
      ),
      # 5th row is for action button
      fluidRow(
        actionButton("go2", "Eintragen", icon = icon("send"))
      ),
      ############################### create modals #################################################
      bsModal(
        "newprodMOD", "Neuen Produktnamen eintragen",
        trigger = "go_prod",
        textInput("newprod", "zukünftiger Produktname"),
        helpText("Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."),
        actionButton("entry_prod", "Produktname eintragen", icon = icon("send"))
      ),
      bsModal(
        "newdelivMOD", "Neue(n) Lieferanten hinzufügen",
        trigger = "go_deliv",
        conditionalPanel(
          condition = "input.deliverer == 'neuer Lieferant'",
          textInput("newdeliv", "zukünftiger Lieferant Nr.1", value = NULL)
        ),
        conditionalPanel(
          condition = "input.deliverer2 == 'neuer Lieferant'",
          textInput("newdeliv2", "zukünftiger Lieferant Nr. 2 (optional)", value = NULL)
        ),
        helpText("Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."),
        actionButton("entry_deliv", "Lieferanten eintragen", icon = icon("send"))
      ),
      bsModal(
        "newgroupMOD", "Neue Produktgruppe hinzufügen",
        trigger = "go_group",
        textInput("newgroup", "zukünftige Produktgruppe"),
        helpText("Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."),
        actionButton("entry_group", "Produktgruppe eintragen", icon = icon("send"))
      ),
      bsModal(
        "newbulkMOD", "Neue VPE hinzufügen",
        trigger = "go_bulk",
        numericInput("newbulk", "zukünftige VPE", min = 0.1, max = 25, step = 0.1, value = 25),
        helpText("Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."),
        actionButton("entry_bulk", "VPE eintragen", icon = icon("send"))
      )
      
    )            
    # end of tab 'neuen Daten abgleichen'
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
  
  #################################################################################################
  ############################ update 'updating starting_csv UI' ##################################
  
  # update all inputs after newproducts when some newproducts changes
  observeEvent(input$newproducts, {
    updateSelectizeInput(
      session, "prodsummary", "Unter folgendem Namen zusammengefasst",
      choices = c("Bitte wählen" = "",
                  levels(starting_csv$Produkte_Zusammenfassung),
                  "neues Produkt")
    )
    updateSelectizeInput(
      session, "deliverer", "Lieferant Nr. 1",
      choices = c("Bitte wählen" = "",
                  levels(starting_csv$Lieferant),
                  "neuer Lieferant")
    )
    updateSelectizeInput(
      session, "deliverer2", "Lieferant Nr. 2",
      choices = c("Bitte wählen" = "",
                  levels(starting_csv$Lieferant))
    )
    updateSelectizeInput(
      session, "prodgroup", "Produktgruppe",
      choices = c("Bitte wählen" = "",
                  levels(starting_csv$Produktgruppe),
                  "neue Produktgruppe")
    )
    updateSelectInput(
      session, "bulksize", "Verpackungseinheit",
      choices = c("Bitte wählen" = "",
                  sort(unique(starting_csv$Verpackungseinheit)),
                  "neue VPE")
    )
  })
  
  # observe go_... butons
  observeEvent(input$entry_prod, {
    toggleModal(session, "newprodMOD", toggle = "close")
    updateSelectizeInput(
      session, "prodsummary", "Unter folgendem Namen zusammengefasst",
      choices = sort(c(levels(starting_csv$Produkte_Zusammenfassung), input$newprod)),
      selected = input$newprod
    )
  })
  observeEvent(input$entry_deliv, {
    toggleModal(session, "newdelivMOD", toggle = "close")
    if (!is.null(input$newdeliv)) {
      updateSelectizeInput(
        session, "deliverer", "Lieferant Nr. 1",
        choices = sort(c(levels(starting_csv$Lieferant), input$newdeliv)),
        selected = input$newdeliv
      )
    }
    if (!is.null(input$newdeliv2)) {
      updateSelectizeInput(
        session, "deliverer2", "Lieferant Nr. 2 (optional)",
        choices = sort(c(levels(starting_csv$Lieferant), input$newdeliv2)),
        selected = input$newdeliv2
      )
    }
  })
  observeEvent(input$entry_group, {
    toggleModal(session, "newgroupMOD", toggle = "close")
    updateSelectizeInput(
      session, "prodgroup", "Produktgruppe",
      choices = sort(c(levels(starting_csv$Produktgruppe), input$newgroup)),
      selected = input$newgroup
    )
  })
  observeEvent(input$entry_bulk, {
    toggleModal(session, "newbulkMOD", toggle = "close")
    updateSelectInput(
      session, "bulksize", "Verpackungseinheit",
      choices = sort(c(unique(starting_csv$Verpackungseinheit), input$newbulk)),
      selected = input$newbulk
    )
  })
  
  # eventReactive: get information about deliverers etc. from starting_csv, if input$prodsummary != 'neues Produkt' 
  prodINFO <- eventReactive(input$prodsummary, {
    if (!input$prodsummary %in% c('neues Produkt', '')) {
      df <- starting_csv[starting_csv$Produkte_Zusammenfassung == input$prodsummary, ]
      # print(df)
      return(df)
    }
  })
  
  # update of deliverer, deliverer2, prodgroup and bulksize when prodsummary is already known
  observeEvent(prodINFO(), {
    if (!input$prodsummary %in% c('neues Produkt', '')) {
      df <- prodINFO()
      # print(df$Lieferant[1])
      # print(levels(starting_csv$Lieferant))
      updateSelectizeInput(
        session, "deliverer", "Lieferant Nr. 1", 
        choices = c(levels(starting_csv$Lieferant),
                    "neuer Lieferant"),
        selected = df$Lieferant[1]
      )
      updateSelectizeInput(
        session, "deliverer2", "Lieferant Nr. 2 (optional)",
        choices = levels(starting_csv$Lieferant),
        selected = df$Lieferant2[1]
      )
      updateSelectizeInput(
        session, "prodgroup", "Produktgruppe",
        choices = c(levels(starting_csv$Produktgruppe),
                    "neue Produktgruppe"),
        selected = df$Produktgruppe
      )
      updateSelectInput(
        session, "bulksize", "Verpackungseinheit",
        choices = c(sort(unique(starting_csv$Verpackungseinheit)),
                    "neue VPE"),
        selected = df$Verpackungseinheit
      )
    }
  })
  
  # enter contant in starting_csv
  enterContant <- eventReactive(input$go2, {
    newrow <- data.frame(
      "Produkte_App" = factor(input$newproducts), 
      "Produkte_Zusammenfassung" = factor(input$prodsummary),
      "Lieferant" = factor(input$deliverer),
      "Lieferant2" = factor(input$deliverer2),
      "Produktgruppe" = factor(input$prodgroup),
      "Verpackungseinheit" = as.numeric(input$bulksize)
    )
    return(newrow)
  })
  
  observeEvent(enterContant(), {
    newrow <- enterContant()
    # add the new row to starting_csv
    starting_csv <<- rbind(starting_csv, newrow)
    # delete row from addStartingCSV where ProdukteApp == input$newproducts
    addStartingCSV <<- addStartingCSV[which(addStartingCSV != input$newproducts)]
    
    # update selectizeInput of newproducts
    updateSelectizeInput(
      session, "newproducts", "Produktname in der App",
      choices = c("Bitte wählen" = "",
                  addStartingCSV)
    )
  })
})

####################################### shiny app executing ################################################
shinyApp(ui, server, options = list(port = 1234))
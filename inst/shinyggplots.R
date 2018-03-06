library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
# der Datesatz totalDistances muss schon eingelesen sein
pal <- colorFactor(c("darkgreen", "orange", "darkred"), domain = c(  "Erzeuger", "Produzent", "Zwischenhaendler"))

pal2 <- colorNumeric(
  palette = "viridis",
  domain = producersInfoStraight$avg.turnover, n = 10, reverse = F)

# Erstellen der beiden Datensätze, zwischen denen gewählt werden kann
turnOver2016 <- totalDistances %>%
  group_by(Produktgruppe)%>%
  summarise(Menge = sum(turnover2016, na.rm = T), Distanz = mean(Gesamtentfernung, na.rm = T))

turnOver2017 <- totalDistances %>%
  group_by(Produktgruppe)%>%
  summarise(Menge = sum(turnover2017, na.rm = T), Distanz = mean(Gesamtentfernung, na.rm = T))

ui <- fluidPage(
   
   titlePanel("Entfernung und Mengen der Produktgruppen"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("Selection", "Selection", c("Alle Produktgruppen", unique(producersInfo$Produktgruppe), selected = "Alle Produktgruppen")),
         hr(),
         uiOutput("ui")#,
        #selectInput("Produkt", "Produkt", 
         #           c("Alle Produkte", unique(producersInfo$Produkte_Zusammenfassung)))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("map", leafletOutput("mymap")),
          tabPanel("ggplot", plotOutput("distPlot"),
                   selectInput("data","Wähle das Jahr", choices = c("2016", "2017")))
        )
         
      )
   )
)

server <- function(input, output, session) {
  #den Jahren aus Choices werden die entsprechenden Datensätze zugewiesen
  dataInput <- reactive({
    data <- switch(input$data,
                   "2016" = turnOver2016,
                   "2017" = turnOver2017)
    title <- ifelse (input$data == "2016", "Jahresumsatz 2016", 
                     "Jahresumsatz 2017")
    return(list("data" = data, "title" = title))
  })
   
   output$distPlot <- renderPlot({
     turnOverYear <- dataInput()$data
     
     # Erstellen des Plots
     ggplot(turnOverYear, aes(Distanz, Menge)) + 
       geom_point(aes(color = Produktgruppe, size = 3)) +
       scale_size(guide = "none") +
       ggtitle(dataInput()$title)
   })
   
   #############################
   

   
   # observe({
   #   x <- input$Selection
   #   
   #   # Can use character(0) to remove all choices
   #   if (is.null(x))
   #     x <- character(0)
   #   
   #   # Can also set the label and select items
   #   updateSelectInput(session, "Produkt",
   #                     label = paste("Produkt"), # , length(x)
   #                     choices = c("Alle Produkte", unique(prodSel()$producersSel$Produkte_Zusammenfassung)),
   #                     selected = input$Produkt)
   # })
   output$ui <- renderUI({
     Produkte <- unique(subset(producersInfo, Produktgruppe == input$Selection)$Produkte_Zusammenfassung)
     if (is.null(input$input_type))
       return(if(input$Selection != "Alle Produktgruppen"){
         radioButtons("Produkt", "Produkt",
                            choices = c("Alle Produkte", Produkte))#, selection = "Alle Produkte"
       } else { radioButtons("Produkt", "Produkt",
                             choices = c("Alle Produkte"))}
       )
     
  
   

     
     # Depending on input$input_type, we'll generate a different
     # UI component and send it to the client.
     
   })
   prodSel <- reactive({
     if(input$Selection == "Alle Produktgruppen"){producersSel <- producersInfo}
     if(input$Selection != "Alle Produktgruppen"){ # & input$Produkt == "Alle Produkte"
       producersSel <- subset(producersInfo, Produktgruppe == input$Selection) 
     } 
     if(input$Selection != "Alle Produktgruppen" & input$Produkt != "Alle Produkte"){
       producersSel <- subset(producersInfo, Produkte_Zusammenfassung == input$Produkt) 
     }
     return(list("producersSel" = producersSel))
   })

   output$mymap <- renderLeaflet({
      prodSelect <- prodSel()$producersSel
      
     leaflet(prodSelect) %>% 
       #addTiles() %>% 
       addProviderTiles(providers$CartoDB.Positron) %>% 
       addPolylines(weight = ifelse((prodSelect$avg.turnover/15) < 1, 1, (prodSelect$avg.turnover)/15),
                    color = ~pal2(prodSelect$avg.turnover),#
                    popup = prodSelect$Produkte_Zusammenfassung) %>% 
       addLegend(pal = pal2, values = ~producersInfo$avg.turnover, title = "avg.turnover",
                labFormat = labelFormat(suffix = " kg/yr")) %>%
       # addCircleMarkers(data = producersExist, radius = 2,
       #                  stroke = FALSE, fillOpacity = 0.8, color=pal(producersExist$Lieferantentyp),
       #                  popup = producersExist$Lieferant) %>%  #, clusterOptions = markerClusterOptions()
       # addLegend("bottomright",
       #           pal = pal, values = ~producersExist$Lieferantentyp,
       #           title = "Lieferantentyp",
       #           opacity = 1
       # ) %>%
       addMarkers(Kornkammer, lng = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2],
                  popup= "Kornkammer")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


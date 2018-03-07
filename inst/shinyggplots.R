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
   
   titlePanel("Die Produkte der FoodCoop Kornkammer"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("Selection", "Auswahl", c("Alle Produktgruppen", unique(producersInfo$Produktgruppe), selected = NULL)),
         hr(),
         uiOutput("ui")#,
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Karte der Produktherkunft", br(),
                   p(em("Die Herkunftsgenauigkeit der Produkte variiert: gestrichelte Linien stellen eine unsichere Herkunftsangabe dar")),
                   leafletOutput("mymap")),
          tabPanel("Umsatz der Produkte", br(), plotOutput("Mengenplot")),
          tabPanel("Transportdistanz der Produkte",  br(), plotOutput("Distanzplot")),
          tabPanel("Transport vs. Umsatz",  br(), plotOutput("distPlot"),
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
   

   output$ui <- renderUI({
     Produkte <- unique(subset(producersInfo, Produktgruppe == input$Selection)$Produkte_Zusammenfassung)
     if (is.null(input$input_type))
       return(if(input$Selection != "Alle Produktgruppen"){
         radioButtons("Produkt", "Produkt",
                            choices = c("Alle Produkte", Produkte))#, selection = "Alle Produkte"
       } else { radioButtons("Produkt", "Produkt",
                             choices = c("Alle Produkte"))}
       )
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
      dashs <- prodSelect$Herkunftsgenauigkeit
      dashs[which(dashs == 2)] <- 1
      dashs[which(dashs == 1)] <- 0
      dash <- as.factor(dashs)
     leaflet(prodSelect) %>% 
       #addTiles() %>% 
       addProviderTiles(providers$CartoDB.Positron) %>% 
       addPolylines(weight = ifelse((prodSelect$avg.turnover/15) < 1, 1, (prodSelect$avg.turnover)/15),
                    color = ~pal2(prodSelect$avg.turnover),#
                    popup = prodSelect$Produkte_Zusammenfassung, dashArray = dash) %>% 
       addLegend(pal = pal2, values = ~producersInfo$avg.turnover, title = "Umsatz pro Jahr",
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
   
   output$Mengenplot <- renderPlot({
     data <- as.data.frame(prodSel()$producersSel)
     
     # Erstellen des Plots
     if(input$Selection == "Alle Produktgruppen"){
       bild <- ggplot(data, aes(Produktgruppe, avg.turnover, fill = Produktgruppe)) + 
         geom_bar(stat = "identity")  + ylab("Durchschnittlicher Umsatz pro Jahr [kg]") +
         theme(axis.text.x = element_text(size=10, angle=45, hjust = 1))
     } else {
       bild <- ggplot(data, aes(Produkte_Zusammenfassung, avg.turnover, fill = Produkte_Zusammenfassung)) + 
         geom_bar(stat = "identity")+ ylab("Durchschnittlicher Umsatz pro Jahr [kg]") +
         theme(axis.text.x = element_text(size=10, angle=45, hjust = 1))
     }
     bild
   })

   output$Distanzplot <- renderPlot({
     data <- as.data.frame(prodSel()$producersSel)
     
     # Erstellen des Plots
     if(input$Selection == "Alle Produktgruppen"){
       bild <- ggplot(data, aes(Produktgruppe, Gesamtentfernung, fill = Produktgruppe)) + 
         geom_bar(stat = "identity")  + 
         theme(axis.text.x = element_text(size=10, angle=45, hjust = 1))
     } else {
       bild <- ggplot(data, aes(Produkte_Zusammenfassung, Gesamtentfernung, fill = Produkte_Zusammenfassung)) + 
         geom_bar(stat = "identity") + 
         theme(axis.text.x = element_text(size=10, angle=45, hjust = 1))
     }
     bild
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


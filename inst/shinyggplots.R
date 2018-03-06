library(shiny)
library(dplyr)
library(ggplot2)

# der Datesatz totalDistances muss schon eingelesen sein

pal2 <- colorNumeric(
  palette = "Spectral",
  domain = producersInfoStraight$avg.turnover, n = 10, reverse = T)

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

        selectInput("data","Wähle das Jahr", choices = c("2016", "2017")),
        radioButtons("display", label = "Selection",
                     c("Alldata", "Selection"), selected = "Alldata"),
         selectInput("Selection", "Selection", unique(producersInfo$Produktgruppe), selected = "Grundnahrungsmittel")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("map", leafletOutput("mymap")),
          tabPanel("ggplot", plotOutput("distPlot"))
        )
         
      )
   )
)

server <- function(input, output) {
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
   
   prodSel <- reactive({
     if(input$display == "Alldata"){producersSel <- producersInfo}
     if(input$display == "Selection"){
       producersSel <- subset(producersInfo, Produktgruppe == input$Selection) 
     }
     return(list("producersSel" = producersSel))
   })
   
   output$mymap <- renderLeaflet({
      prodSelect <- prodSel()$producersSel
      
     leaflet(producersInfo) %>% 
       addTiles() %>% 
       addPolylines(weight = 0.4,
                    color = ~pal2(producersInfo$avg.turnover),
                    popup = producersInfo$Produkte_Zusammenfassung) %>% 
       addLegend(pal = pal2, values = ~producersInfo$avg.turnover, title = "avg.turnover",
                 labFormat = labelFormat(suffix = " kg/yr")) %>% 
       addPolylines(data = prodSelect, color =  ~pal2(producersInfo$avg.turnover),
                    weight = (1/producersInfo$Herkunftsgenauigkeit)*6,
                    popup = producersInfo$Produkte_Zusammenfassung)
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


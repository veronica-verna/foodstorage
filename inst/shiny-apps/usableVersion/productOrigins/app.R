library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  selectInput("Selection", "Selection", unique(producersInfo$Produktgruppe), selected = "Grundnahrungsmittel")
)

server <- function(input, output, session) {
  
  prodSel <- reactive({
    producersSel <- subset(producersInfo, Produktgruppe == input$Selection)  
    return(list("producersSel" = producersSel))
  })
  
  
  
  output$mymap <- renderLeaflet({
    producersInfo <- prodSel()$producersSel
    leaflet(producersInfo) %>% 
      addTiles() %>% 
      addPolylines(weight = (1/producersInfo$Herkunftsgenauigkeit)*6,
                   color = ~pal2(producersInfo$avg.turnover),
                   popup = producersInfo$Produkte_Zusammenfassung) %>% 
      addLegend(pal = pal2, values = ~producersInfo$avg.turnover, title = "avg.turnover",
                labFormat = labelFormat(suffix = " kg/yr")) 
    
  })
  
  
}

shinyApp(ui, server)
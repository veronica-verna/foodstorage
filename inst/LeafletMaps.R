
library(leaflet)

pal <- colorFactor(c("green", "orange", "red"), domain = c(  "Erzeuger", "Produzent", "Zwischenhaendler"))

leaflet(producersExist) %>%
  addTiles() %>%
  addCircleMarkers(radius = 6,
                   stroke = FALSE, fillOpacity = 0.8, color=pal(producersExist$Lieferantentyp),
                   popup = producersExist$Lieferant) %>%  #, clusterOptions = markerClusterOptions()
  addLegend("bottomright",
            pal = pal, values = ~producersExist$Lieferantentyp,
            title = "Lieferantentyp",
            opacity = 1
  ) %>%
  addMarkers(Kornkammer, lng = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2],
             popup= "Kornkammer")



########################

producersInfoStraight$avg.turnover <- as.numeric(producersInfoStraight$avg.turnover)

pal2 <- colorNumeric(
  palette = "Spectral",
  domain = producersInfoStraight$avg.turnover, n = 10, reverse = T)

leaflet(producersRoutes) %>% 
  addTiles() %>% 
  addPolylines(color = "brown") %>% 
  addPolylines(data = producersInfoStraight, color = ~pal2(producersInfoStraight$avg.turnover),
               weight = (1/producersInfoStraight$Herkunftsgenauigkeit)*3,
               popup = producersInfoStraight$Produkte_Zusammenfassung,
               smoothFactor = 0.2, fillOpacity = 1) %>% 
  addLegend(pal = pal2, values = ~producersInfoStraight$avg.turnover, title = "avg.turnover",
            labFormat = labelFormat(suffix = " kg/yr"))


leaflet(producersInfo) %>% 
  addTiles() %>% 
  addPolylines(weight = (1/producersInfoStraight$Herkunftsgenauigkeit)*6,
               color = ~pal2(producersInfo$avg.turnover),
               popup = producersInfo$Produkte_Zusammenfassung) %>% 
  addLegend(pal = pal2, values = ~producersInfo$avg.turnover, title = "avg.turnover",
            labFormat = labelFormat(suffix = " kg/yr"))

#################################

library(leaflet)

pal <- colorFactor(c("green", "orange", "red"), domain = c(  "Erzeuger", "Produzent", "Zwischenhaendler"))


KKIcon <- iconList(
  Kornkammer = makeIcon("data/icon-2.png", "data/icon-2.png", 24, 24)
)

leaflet(producersExist) %>%
  addTiles() %>%
  addCircleMarkers(radius = 6,
                   stroke = FALSE, fillOpacity = 0.8, color=pal(producersExist$Lieferantentyp),
                   popup = producersExist$Lieferant) %>%  #, clusterOptions = markerClusterOptions()
  addLegend("bottomright",
            pal = pal, values = ~producersExist$Lieferantentyp,
            title = "Lieferantentyp",
            opacity = 1
  ) %>%
  addMarkers(Kornkammer,
             lng = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2],
             icon =  ~KKIcon[Kornkammer$Name],
             popup= "Kornkammer") %>% 
  addPolylines(data = producersInfo, weight = 2, 
               popup = producersInfo$Produkte_Zusammenfassung)


leaflet(Kornkammer) %>% 
  addTiles() %>% 
  addMarkers(icon =  ~KKIcon[Name])
####################
oceanIcons <- iconList(
  ship = makeIcon("data/icon-2.png", "data/icon-2.png", 18, 18),
  pirate = makeIcon("data/logo-rund-small.png", "data/logo-rund-small@2x.png", 24, 24)
)
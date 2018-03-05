
library(osrm)
## Function to construct a lines-file for every product to its origin:

createLines <- function(i, tD = totalDistances, pEx = producersExist, pOE = productOriginExist){
  productOriginExist <- pOE
  producersExist <- pEx
  totalDistances <- tD
  pE <- which(producersExist$Lieferant == totalDistances$Lieferant[i])
  if(totalDistances$Lieferantentyp[i] == "Zwischenhaendler" & 
     totalDistances$Lieferant[i] %in% unique(productOriginExist$Lieferant)){
    
    z <- which(productOriginExist$Lieferant == totalDistances$Lieferant[i] &
                 productOriginExist$Produkte_Zusammenfassung == totalDistances$Produkte_Zusammenfassung[i] &
                 productOriginExist$Ort == totalDistances$Ort[i])
    Lines(Line(rbind(coordinates(Kornkammer), coordinates(producersExist)[pE,], coordinates(productOriginExist)[z,])), ID = i)
  } else {
    Lines(Line(rbind(coordinates(Kornkammer), coordinates(producersExist)[pE,])), ID = i)
  }
}
## create line for every path to the Kornkammer:
linefile <- lapply(1:nrow(totalDistances), createLines)

producers <- SpatialLines(linefile, proj4string = crs(producersExist)) #crs(producersExist)
producersInfo <- SpatialLinesDataFrame(producers, totalDistances)


# Get route from OSM: package osrm
## transform the data to the needed format for the osrmRoute()-function 
KoKa <- data.frame(id = Kornkammer$Name, lon = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2])
prodEx <- data.frame(id = producersExist$Lieferant, lon = coordinates(producersExist)[,1], coordinates(producersExist)[,2])

lineRoutes <- lapply(1:nrow(producersExist), function(x) Lines(Line(osrmRoute(KoKa, prodEx[x,], sp = F)), ID = x))
row.names(prodEx) <- as.character(1:19)
lR <- SpatialLines(lineRoutes, proj4string= crs(producersExist))
producersRoutes <- SpatialLinesDataFrame(lR, prodEx)


leaflet(producersRoutes) %>% 
  addTiles() %>% 
  addPolylines(color = "brown") %>% 
  addPolylines(data = producersInfoFlight, color = "brown", 
               weight = producersInfoFlight$avg.turnover/10,
               popup = producersInfoFlight$Produkte_Zusammenfassung)



leaflet(producersInfo) %>% 
  addTiles() %>% 
  addPolylines()
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

# Some fake data
df <- sp::SpatialPointsDataFrame(
  cbind(
    (runif(20) - .5) * 10 - 90.620130,  # lng
    (runif(20) - .5) * 3.8 + 25.638077  # lat
  ),
  data.frame(type = factor(
    ifelse(runif(20) > 0.75, "pirate", "ship"),
    c("ship", "pirate")
  ))
)

leaflet(df) %>% addTiles() %>%
  # Select from oceanIcons based on df$type
  addMarkers(icon = ~oceanIcons[type])


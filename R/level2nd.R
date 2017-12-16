#'@title level2nd 
#'@description generates 2nd level for shiny UI
#'@export 

level2ndFUN <- function(kornumsatz, product.group, deliverers){
  list("ONEprod" = as.character(levels(kornumsatz$Produkt)), # 'Einzelnes Produkt' in UI
       "family" = product.group, # 'Produktgruppe (Familie)' in UI
       "producer" = deliverers) #, 'Lieferanten' in UI
  # "producer2" = deliverers2)
}
  
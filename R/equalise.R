#' @export

equalise <- function(data, startingCSV) {
  
  # check out which products are new
  newproducts <- levels(data$Produkt)[-which(levels(data$Produkt) %in% levels(startingCSV$Produkte_App))]
  return(newproducts)
  
  
  # newProdukte_App <- c(levels(startingCSV$Produkte_App), newproducts) %>%
  #   sort() 
  # return(newProdukte_App)
  
  
}
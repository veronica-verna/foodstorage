#' @export

equalise <- function(oldData, newData, startingCSV) {
  # equalise colnames of data
  colnames(newData) <- colnames(oldData)
  
  # check out which products are new
  newproducts <- levels(newData$Produkt)[-which(levels(newData$Produkt) %in% levels(oldData$Produkt))]
  return(newproducts)
  
  
  # newProdukte_App <- c(levels(startingCSV$Produkte_App), newproducts) %>%
  #   sort() 
  # return(newProdukte_App)
  
  
}
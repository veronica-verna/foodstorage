#' @export

equalise <- function(oldData, newData, startingCSV) {
  # equalise colnames of data
  colnames(newData) <- colnames(oldData)
  
  # check out which products are new
  newproducts <- levels(newData$Produkt)[-which(levels(newData$Produkt) %in% levels(oldData$Produkt))]
  len <- length(newproducts)
  # make a data.frame consisting of the new products...
  addStartingCSV <- data.frame(newproducts, factor(rep("", len)), factor(rep("", len)), 
                               factor(rep("", len)), factor(rep("", len)), numeric(len))
  colnames(addStartingCSV) <- colnames(startingCSV)
  return(addStartingCSV)
  
  
  # newProdukte_App <- c(levels(startingCSV$Produkte_App), newproducts) %>%
  #   sort() 
  # return(newProdukte_App)
  
  
}
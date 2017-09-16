#' @export
prepare <- function(name.of.product,
                    data = get("kornumsatz"),
                    result = "timespan",
                    class.of.product = "character",
                    correction = 0.05,
                    test = FALSE) {
  
  ##### at first: check if the input is correct! ####
  if (is.data.frame(data) == FALSE)
    stop("data must be a data frame with 10 columns. For details type help(prepare).")
  if ("Datum" %in% colnames(data)) {
    dates <- data$Datum
  } else stop("prepare function is using wrong 'kornumsatz'")
  products <- data$Produkt
  comul.change <- data$MengeKum
  food.storage <- data$Bestand_Einheit
  VPE <- data$VPE
  
  # for is.Date; cheeck if dates is a Date
  if (!is.Date(dates)) stop("Your vector of Dates has to be in format as.Date()")
  
  # Only factors are allowed for the vector of products!
  if (!is.factor(products)) stop("Vector of products has to be a factor")
  
  # name of product has to be in vector of products!
  lev <- levels(products)
  if (length(name.of.product) == 1) {
    if (!isTRUE(name.of.product %in% lev)) stop("Vector of products has to contain name.of.product")
  } 
  
  ########################################## subset dataframe #####################################
  # create dataframe only with one product
  sub.df <- subset(data, Produkt == name.of.product)  %>%
   correction(VPE[1]) -> sub.df
  if (class.of.product == "character") sub.df$Produkt <- as.character(sub.df$Produkt)
  
  ################ start correction

  # only current storage is interesting...
  if (result == "current") sub.df <- sub.df[nrow(sub.df), ]
    
  return(sub.df)
}  
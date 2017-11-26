#' @export
prepare <- function(name.of.product,
                    data = get("kornumsatz"),
                    result = "timespan",
                    class.of.product = "character",
                    correction = 0.05,
                    test_correction = FALSE) {
  
  ##### at first: check if the input is correct! ####
  if (!is.data.frame(data))
    stop("data must be a data frame with 10 columns. For details type help(prepare).")
  if ("Tag" %in% colnames(data)) {
    dates <- data$Tag
  } else stop("prepare function is using wrong 'kornumsatz'")
  products <- data$Produkt
  comul.change <- data$Menge
  food.storage <- data$Bestand.Einheit
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
  sub.df <- subset(data, Produkt == name.of.product)
  if (isTRUE(test_correction)) return(sub.df) # for debuggin with test_that
  # sub.df <- correction(sub.df, sub.df$VPE[1])
  if (class.of.product == "character") sub.df$Produkt <- as.character(sub.df$Produkt)
  
  ################ start correction

  # only current storage is interesting...
  if (result == "current") sub.df <- sub.df[nrow(sub.df), ]
  
  # only members' shopping
  if (result == "shopping") {
    from = sub.df[which(sub.df$Menge > 0), ]$Tag[1]
    to = dates[length(dates)]
    Einheit = sub.df[1, ]$Einheit
    sub.df <- sub.df[sub.df$Menge < 0, ]
    sub.df <- data.frame(
      "Von" = from,
      "Bis" = to,
      "Produkt" = name.of.product,
      "Verbrauch" = ifelse(
        nrow(sub.df) == 0,
        yes = 0, # nothing was bought
        no = abs(sum(sub.df$Menge)) # sum.shopping
      ), 
      "Einheit" = Einheit
    )
  }
    
  return(sub.df)
}  
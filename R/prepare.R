#' @export
prepare <- function(name.of.product,
                    what.plotting = "regression",
                    from = "",
                    to = "",
                    correction = 0.05, 
                    more.than = 15,
                    data = get("kornumsatz"),
                    current.storage = FALSE,
                    myPlot = FALSE,
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
  
    require(lubridate)
    # for is.Date; cheeck if dates is a Date
    if (is.Date(dates) == FALSE) 
      stop("Your vector of Dates has to be in format as.Date()")
  
    # Only factors are allowed for the vector of products!
    if (is.factor(products) == FALSE) 
      stop("Vector of products has to be a factor")
    
    # name of product has to be in vector of products!
    lev <- levels(products)
    if (length(name.of.product) == 1) {
      if (!isTRUE(name.of.product %in% lev)) 
        stop("Vector of products has to contain name.of.product")
    } else {
      for (i in 1:length(name.of.product)) {
        if (isTRUE(name.of.product[i] %in% lev) == FALSE) 
          stop("Vector of products has to contain ALL name.of.product[s]")
      }
    }
    
    # possibilites for "what.plotting"
    possibilities <- c("allall", "alles", "Verzehr", "Warenbestand", "regression")
    if (isTRUE(what.plotting %in% possibilities) == FALSE) 
      stop("what.plotting has to be a character: regression, allall, all.verzehr, all.lager, Verzehr or Warenbestand!")
  
  ##### create sortbydays and sortbypos ####
    Datum <- seq(from=range(dates)[1], 
                 to=range(dates)[2], 
                 by="day") 
    Tag_Nr <- 1:length(Datum)
    sortbydays <- data.frame(Datum, Tag_Nr)
    
    # create sortbypos - cbind "Produkt" with "Position" ####
    Position <- 1:length(products)
    sortbypos <- data.frame(Position = Position, Produkt = products)
    sortbypos <- cbind(sortbypos, Datum = dates[sortbypos$Position])
  
  
  ##### What do you wanna plot? #####
    # one product - consumption | food.storage | both | regression
    # first of all: security checks ##
    if (is.numeric(comul.change) == FALSE) 
      stop("To plot consumption, at least daily change is necessary (as data type numeric)")
    if (length(comul.change[sortbypos$Position]) != nrow(sortbypos)) 
      stop("length differ! Probably you didn't specify the product for comulative change")
    # checks only for storage and "all"
    if (what.plotting == "alles" | what.plotting == "Warenbestand" | what.plotting == "regression") {
      if (is.numeric(food.storage) == FALSE) 
        stop("If you want to plott food storage, you need a vector of storage. Of course, same length as the comulative one.")
      if (length(food.storage) != length(dates)) 
        stop("length differ! Probably you didn't specify food storage.")
    } 
    
    #### start here: sortbypos only for name.of.product ####
    sortbypos <- sortbypos[sortbypos$Produkt == name.of.product, ] 
    sortbypos <- cbind(sortbypos, 
                       MengeKum = comul.change[sortbypos$Position])
    if (what.plotting == "alles" | what.plotting == "Warenbestand" | what.plotting == "regression") {
      sortbypos <- cbind(sortbypos,
                         Bestand_Einheit=food.storage[sortbypos$Position]) 
    }
    if (what.plotting == "regression") VPE <- VPE[sortbypos$Position][1]
    
    #### join daily change to sortbypos ####
    # merge sortbypos and sortbydays, by days of course
    require(data.table) # v1.9.5+
    sortbydays <- merge(sortbydays, sortbypos, by='Datum', all=T)
    
    # fill NAs with numbers
    sortbydays$MengeKum[is.na(sortbydays$MengeKum)] <- 0
    if (what.plotting == "alles" | what.plotting == "Warenbestand" | what.plotting == "regression") {
      for (i in 1:nrow(sortbydays)) sortbydays$Bestand_Einheit[i] <- sum(sortbydays$MengeKum[1:i])
    }
    
    
    ## make the '5% correction' if necessary##
    if (what.plotting == "regression") sortbydays <- correction(sortbydays, VPE, correction = correction, more.than = more.than)
    
    #### Create the consumption ####
    if (what.plotting == "alles" | what.plotting == "Verzehr") {
      sortbydays <- cbind(sortbydays, MengeKonsum = numeric(nrow(sortbydays)))
      sortbydays[sortbydays$MengeKum <= 0, ]$MengeKonsum <- abs(sortbydays[sortbydays$MengeKum <= 0, ]$MengeKum)
      sortbydays[sortbydays$MengeKum > 0, ]$MengeKonsum <- 0
    }
    
    #### and here are the results depending on what you wanna plot ####
    if (what.plotting == "alles") {
      table <- data.frame(Datum = sortbydays$Datum, 
                          Tag_Nr = sortbydays$Tag_Nr, 
                          Warenbestand = sortbydays$Bestand_Einheit,
                          Verzehr = sortbydays$MengeKonsum)
    }
    if (what.plotting == "Verzehr") {
      table <- data.frame(Datum = sortbydays$Datum, 
                          Tag_Nr = sortbydays$Tag_Nr, 
                          Verzehr = sortbydays$MengeKonsum)
    }
    if (what.plotting == "Warenbestand") {
      table <- data.frame(Datum = sortbydays$Datum, 
                          Tag_Nr = sortbydays$Tag_Nr, 
                          Warenbestand = sortbydays$Bestand_Einheit)
    }
    if (what.plotting == "regression") {
      if (is.numeric(VPE) == FALSE) 
        stop("VPE must be a numeric vector with same length than all other vectors.")
      table <- data.frame(Datum = sortbydays$Datum,
                          Tag_Nr = sortbydays$Tag_Nr,
                          Warenbestand = sortbydays$Bestand_Einheit,
                          MengeDif = sortbydays$MengeKum,
                          VPE = rep(VPE, nrow(sortbydays)))
    }
    
    #### check if from/to is set ####
    if (from == "") { # means: standard/usual case
      from <- table$Datum[nrow(table)] %m-% months(6)
      table <- table[table$Datum >= from, ]
    } else { 
      if (is.character(from) == FALSE) stop("'From' must be a character string in format 'yyyy-mm-dd'")
      from <- as.Date(from, origin = "1970-01-01")
      table <- table[table$Datum >= from,]
    }
    if (to != "") {
      if (is.character(to) == FALSE) stop("'To' must be a character string in format 'yyyy-mm-dd'")
      to <- as.Date(to, origin = "1970-01-01")
      if (from > to) stop("'From' must be before 'to'")
      table <- table[table$Datum <= to,]
    }
    
    if (current.storage == TRUE) table <- table[nrow(table), ]
    
    if (test == TRUE) return("yes")
    if (myPlot == TRUE) return(list(table = table, name.of.product = name.of.product))
    return(table)
    
}
#' @export
#### function for multiplying different other functions

multiply <- function(FUN, 
                     group, 
                     par = list(what.plotting = "Warenbestand", from = Sys.Date() - months(6), to = Sys.Date, more.than = 15, correction = 0.05), 
                     data = get("kornumsatz"), 
                     current.storage = TRUE, 
                     reduce = FALSE,
                     test = FALSE) {
  
  if (class(FUN) != "function") stop("FUN must be of class 'function'")
  FUN <- match.fun(FUN)
  #fun.name <- tolower(FUN)
  if (is.data.frame(data) == FALSE)
    stop("data must be a data frame with 10 columns. For details type help(prepare).")
  dates <- data$Datum
  products <- data$Produkt
  comul.change <- data$MengeKum
  food.storage <- data$Bestand_Einheit
  VPE <- data$VPE
  
  
  # security checks first: 
  # Only factors are allowed for the vector of products!
  if (is.factor(products) == FALSE) 
    stop("Vector of products has to be a factor")
  
  lev <- levels(products)
  group_size <- length(group)
  if (length(unique(group %in% lev)) !=1 && unique(group %in% lev) != TRUE)
    stop("Vector of products has to contain ALL name.of.product[s]")
   
  #if (fun.name == "prepare") {
    # check, if prepare gets all necessary parameter
  necessary <- c("what.plotting", "from", "to", "more.than", "correction")
  if (!(names(par) %in% necessary)) 
    stop("par must be a list consisting of following arguments: what.plotting, from, to, more.than, correction")
    
    if (par$what.plotting == "Warenbestand") {
      for (i in 1:group_size) {
        if (i == 1) {
          table <- prepare(group[i], par$what.plotting, par$from, par$to, par$correction, par$more.than, current.storage = current.storage)
          colnames(table)[3] <- group[i]
        } else {
          new.column <- prepare(group[i], par$what.plotting, par$from, par$to, par$correction, par$more.than, current.storage = current.storage)[,3]
          if (nrow(table) != length(new.column)) 
            stop("products of this group have not the same length")
          table <- cbind(table, new.column = new.column)
          colnames(table)[i + 2] <- group[i]
        } 
      } 
      
      if (reduce == TRUE) table <- data.frame(Datum = table[,1], 
                                               Warenbestand = rowSums(table[, 3:ncol(table)]))
      
      if (reduce == FALSE && current.storage == TRUE) {
        Warenbestand <- t(table[,-c(1,2)])
        colnames(Warenbestand)[1] <- "Kilo"
        full <- Warenbestand[Warenbestand[,1] > 0.03, ]
        empty <- Warenbestand[Warenbestand[,1] <= 0.03, ]
        
        if (length(empty) == 1) names(empty) <- rownames(Warenbestand)[which(Warenbestand[,1] %in% empty)]
        if (length(full) == 1) names(full) <- rownames(Warenbestand)[which(Warenbestand[,1] %in% full)]
        
        if (test == TRUE) return("yes")
        return(list(Warenbestand = full, Leer = names(empty), Datum = table$Datum[1]))
      }
    }
    
  
    if (test == TRUE) return("yes")
    return(table)
  #}
}
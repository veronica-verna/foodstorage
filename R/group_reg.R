#### function for calculating regressions for groups ####

group_reg <- function(group, from = "", to = "", list = FALSE) {
  
  len <- length(group)
  
  ## if group is bigger than 12, result will be a data frame ###
  
  if (len > 12 | list == TRUE) {
    # create an empty data.frame and empty name vector
    table <- data.frame(Noch4Wochen = as.Date(character()), Ende = as.Date(character()))
    Produkt <- c()
    
    for (i in 1:len) {
      # ERROR HANDLING
      possibleError <- tryCatch(fun_reg(group[i], graphics = FALSE), error = function(e) e)
      
      if (!inherits(possibleError, "error")) {
        # REAL WORK
        table[i, ] <- fun_reg(group[i], graphics = FALSE)
        Produkt <- c(Produkt, group[i])
      }
      
      
    }
    
    table <- cbind(Produkt, table)
    
    return(table)
  } 
  
  ### plot-window settings ###
  if (len == 2) par(mfrow=c(2,1))
  if (len %in% c(3,4)) par(mfrow=c(2,2))
  if (len >= 5) par(mfrow=c(ceiling(len/3), 3))
  
  # now plotting ##
  for (i in 1:len) fun_reg(group[i], from = from, to = to)
  par(mfrow = c(1,1))
}
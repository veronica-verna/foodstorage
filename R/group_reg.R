#### function for calculating regressions for groups ####

group_reg <- function(group, from = "", to = "", list = FALSE) {
  
  len <- length(group)
  
  ## if group is bigger than 12, result will be a data frame ###
  
  if (len > 12 | list == TRUE) {
    # create an empty data.frame and empty name vector
    table.works <- data.frame(Produkt = character(), Noch4Wochen = as.Date(character()), Ende = as.Date(character()))
    big.list <- lapply(group, product.is.over)
    all.errors <- vector("list", length = 0)
    # seperate 'works' from 'errors'
    for (i in 1:len) {
      if (is.data.frame(big.list[[i]]) == TRUE) {
        table.works <- rbind(table.works, big.list[[i]])
      } else {
        all.errors[[length(all.errors) +1]] <- big.list[[i]] 
      }
    }
    # next step: which different errors do we have and give each of them a number
    dif.error.calls <- unique(unlist(lapply(all.errors, '[[', 2)))
    dif.error.messages <- unique(unlist(lapply(all.errors, '[[', 3)))
    table.errors <- data.frame(Produkt = character(), error.call = character(), error.message = character())
    ## put error categories in a table!
    for (i in 1:length(all.errors)) {
      table.errors[i, 1] <- all.errors[[i]][[1]]
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
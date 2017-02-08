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
    # now first output is finished: table.works
    # next step: which different errors do we have and give each of them a number
    all.error.calls <- unlist(as.character(lapply(all.errors, '[[', 2)))
    dif.error.calls <- unique(all.error.calls)
    dif.error.messages <- unique(unlist(lapply(all.errors, '[[', 3)))
    
    ## put error categories in a table!
    products <- unlist(lapply(all.errors, '[[', 1))
    call.vector <- character()
    message.vector <- character()
    for (i in 1:length(all.errors)) {
      call.vector[i] <- paste0("Call", which(dif.error.calls %in% all.error.calls[i]))
      message.vector[i] <- paste0("Message", which(dif.error.messages %in% all.errors[[i]][[3]])) 
    }
    table.errors <- data.frame(Produkt = products, error.call = call.vector, error.message = message.vector)
    # now second output is finished: table.errors
    # next step: make a legend
    dif.calls <- unique(call.vector)
    dif.messages <- unique(message.vector)
    calls <- vector('list', 0)
    messages <- vector('list', 0)
    for (i in 1:length(dif.error.calls)) calls[length(calls) + 1] <- paste(dif.calls[i], dif.error.calls[i], sep = " = ")
    for (i in 1:length(dif.error.messages)) messages[length(messages) + 1] <- paste(dif.messages[i], dif.error.messages[i], sep = " = ")
    legend <- list(calls = calls, messages = messages) # and that's the last output
    
    return(list(Funktioniert = table.works, Fehler = table.errors, FehlerLegende = legend))
  } 
  
  ### plot-window settings ###
  if (len == 2) par(mfrow=c(2,1))
  if (len %in% c(3,4)) par(mfrow=c(2,2))
  if (len >= 5) par(mfrow=c(ceiling(len/3), 3))
  
  # now plotting ##
  for (i in 1:len) fun_reg(group[i], from = from, to = to)
  par(mfrow = c(1,1)) # for resetting settings
}
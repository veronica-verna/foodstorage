#### function for calculating regressions for groups ####

group_reg <- function(group, from = "", to = "", list = FALSE, filter = TRUE, weeks = 4) {
  
  len <- length(group)
  
  ## if group is bigger than 12, result will be a data frame ###
  
  if (len > 12 | list == TRUE) {
    # create an empty data.frame and empty name vector
    table.works <- data.frame(Produkt = character(), 
                              LetztesAuffuellen = as.Date(character()),
                              Noch4Wochen = as.Date(character()), 
                              Ende = as.Date(character()), 
                              LetzterDatenpunkt = as.Date(character()))
    big.list <- lapply(group, product.is.over, from = from, to = to)
    all.errors <- vector("list", length = 0)
    # seperate 'works' from 'errors'
    for (i in 1:len) {
      if (is.data.frame(big.list[[i]]) == TRUE) { # data.frame, because 'works' returns a data.frame with 3 columns
        table.works <- rbind(table.works, big.list[[i]])
      } else {
        all.errors[[length(all.errors) +1]] <- big.list[[i]] 
      }
    }
    
    # Let's filter table.works
    if (filter == TRUE) {
      # beginning from last data point, show all products which will be over in the next '4' weeks
      last.data.point <- max(table.works$LetzterDatenpunkt)
      referre.date <- last.data.point + weeks(weeks)
      table.works <- table.works[table.works$Ende <= referre.date,]
      table.works <- table.works[with(table.works, order(Ende)), ] # sorting data decreasing end date
      
      # now seperate: which product is completely over?
      if (nrow(table.works[table.works$Ende <= last.data.point,]) >= 1) {
        table.works.over <- table.works[table.works$Ende <= last.data.point,]
        already.over <- data.frame(Produkt = table.works.over$Produkt, 
                                   Bezeichnung = rep("ist leer seit", len = nrow(table.works.over)),
                                   Ende = as.character(table.works.over$Ende),
                                   Bezeichnung = rep("und hielt für", len = nrow(table.works.over)),
                                   Dauer = as.numeric(table.works.over$Ende - table.works.over$LetztesAuffuellen, 
                                                      units = "days"),
                                   Einheit = rep("Tage", nrow(table.works.over)),
                                   check.names = FALSE)
        table.works.over.soon <- table.works[table.works$Ende > last.data.point, ]
        will.be.over.soon <- data.frame(Produkt = table.works.over.soon$Produkt,
                                        Bezeichnung = rep("wird leer sein am", len = nrow(table.works.over.soon)),
                                        Ende = as.character(table.works.over.soon$Ende),
                                        Bezeichnung = rep("also in", len = nrow(table.works.over.soon)),
                                        Dauer = as.numeric(table.works.over.soon$Ende - last.data.point, units = "days"),
                                        Einheit = rep("Tage", len = nrow(table.works.over.soon)),
                                        check.names = FALSE)
        if (nrow(will.be.over.soon) == 0) 
          will.be.over.soon <- paste("In den nächsten", weeks, "Wochen wird nichts ausgehen")
      } # only if there is a 'already-finished-product'
    }
    
    if (exists("already.over") == TRUE && length(all.errors) == 0) 
      return(list(schon.leer = already.over, bald.leer = will.be.over.soon))
    if (length(all.errors) == 0) return(list(bald.leer = will.be.over.soon))
             
    # next step: which different errors do we have and give each of them a number
    all.error.calls <- unlist(as.character(lapply(all.errors, '[[', 2))) # seperate calls and change them to character
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
    table.errors <- data.frame(Produkt = products, Fehlerort = call.vector, Fehlermeldung = message.vector)
    # now second output is finished: table.errors
    # next step: make a legend
    dif.calls <- unique(call.vector)
    dif.messages <- unique(message.vector)
    calls <- vector('list', 0)
    messages <- vector('list', 0)
    for (i in 1:length(dif.error.calls)) calls[length(calls) + 1] <- paste(dif.calls[i], dif.error.calls[i], sep = " = ")
    for (i in 1:length(dif.error.messages)) messages[length(messages) + 1] <- paste(dif.messages[i], dif.error.messages[i], sep = " = ")
    legend <- list(calls = calls, messages = messages) # and that's the last output
    
    if (exists("already.over") == TRUE) return(list(schon.leer = already.over,
                                                    bald.leer = will.be.over.soon,
                                                    manuell.checken = table.errors,
                                                    FehlerLegende = legend))
    return(list(bald.leer = table.works, manuell.checken = table.errors, FehlerLegende = legend))
  }
  
  ############################ if result should be plots ##############################
  
  ### plot-window settings ###
  if (len == 2) par(mfrow=c(2,1))
  if (len %in% c(3,4)) par(mfrow=c(2,2))
  if (len >= 5) par(mfrow=c(ceiling(len/3), 3))
  
  # now plotting ##
  for (i in 1:len) plotting.groups(group[i], from = from, to = to)
  par(mfrow = c(1,1)) # for resetting settings
}
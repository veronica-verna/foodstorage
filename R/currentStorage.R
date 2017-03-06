currentStorage <- function(group, 
                           xlab = "Warenbestand in Kilo",
                           mar = c(4,8,1,1),
                           las = 1,
                           horiz = T,
                           col = "grey",
                           cex.axis = 0.8,
                           cex.names = 0.8,
                           decreasing = TRUE) {
  par(mar = mar)
  
  if (is.list(group)) {
    len <- length(group)
    empty <- character()
    stock <- numeric(length = len)
    for (i in 1:len) {
      group.stock <- multiply(prepare, group[[i]])
      empty[length(empty) + 1] <- names(group.stock$Leer)
      stock[i] <- as.numeric(names(group)[i] = sum(group.stock$Warenbestand))
    }
  } else {
    group.stock <- multiply(prepare, group)
    empty <- names(group.stock$Leer)
    stock <- group.stock$Warenbestand
  }
  
  barplot(sort(stock, decreasing = decreasing), 
          horiz = horiz,
          las = las,
          cex.axis = cex.axis,
          cex.names = cex.names,
          xlab = xlab)
  if (length(empty) != 0) { # usual usecase
    legend("topright", 
           legend = c("Derzeit vergriffen:", empty),
           pch = c(NA, rep(16, length(empty))))
    }
  par(mar = c(5, 4, 4, 2) + 0.1)
}
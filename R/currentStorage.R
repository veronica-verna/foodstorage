#' @export
currentStorage <- function(group, 
                           xlab = "Warenbestand in Kilo",
                           mar = c(4,8,1,1),
                           las = 1,
                           horiz = T,
                           col = "grey",
                           cex.axis = 0.8,
                           cex.names = 0.8,
                           decreasing = TRUE, 
                           summary = FALSE) {
  
  par(mar = mar)
  group.stock <- multiply(prepare, group)
  ### for the 'summary-case'
  if (summary == TRUE) return(list(group.stock$Warenbestand, group.stock$Leer))
  
  barplot(sort(group.stock$Warenbestand, decreasing = decreasing), 
          horiz = horiz,
          las = las,
          cex.axis = cex.axis,
          cex.names = cex.names,
          xlab = xlab)
  if (length(group.stock$Leer) != 0) { # usual usecase
    legend("topright", 
           legend = c("Derzeit vergriffen:", group.stock$Leer),
           pch = c(NA, rep(16, length(group.stock$Leer))))
    }
  par(mar = c(5, 4, 4, 2) + 0.1)
}
#' @export
currentStorage <- function(group, plot = TRUE, horiz = FALSE, fill = TRUE, decreasing = TRUE) {
  
  big.list <- lapply(group, FUN = prepare, result = "current")
  big.df <- do.call(rbind, big.list)
  big.df <- big.df[order(big.df$Bestand_Einheit, decreasing = decreasing),]
  if (plot == FALSE) return(big.df)
  # create ordered factor which is necessary for ggplot2
  big.df$Produkt <- factor(big.df$Produkt, levels = big.df$Produkt)
  
  # create ggplot object
  pplot <- ggplot(big.df, aes(x = factor(Produkt), y = Bestand_Einheit)) +
    theme(plot.title = element_text(hjust = 0.5)) + # to center title
    geom_col() + 
    labs(title = "Aktueller Warenbestand", x= "Produkte", y = "Warenbestand in Kilo") +
    guides(fill = FALSE)
  
  # coloured bars or black-white: create ggplot object
  if (fill == TRUE) pplot <- pplot + aes(fill = factor(Produkt))
  
  # horizontal or vertical
  if (horiz == TRUE) pplot <- pplot + coord_flip()
    else {
      pplot <- pplot + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
        coord_cartesian(ylim = c(0,max(big.df$Bestand_Einheit)))
    }
  
  # renderPlot...
  return(pplot)  
}
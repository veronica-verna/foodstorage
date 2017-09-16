#' @export
currentStorage <- function(group, plot = TRUE, horiz = FALSE, fill = TRUE) {
  
  big.list <- lapply(group, FUN = prepare, result = "current")
  big.df <- do.call(rbind, big.list)
  big.df <- big.df[order(big.df$Bestand_Einheit, decreasing = T),]
  if (plot == FALSE) return(big.df)
  # create ordered factor
  big.df$Produkt <- factor(big.df$Produkt, levels = big.df$Produkt)
  
  # coloured bars or black-white: create ggplot object
  if (fill == TRUE) {
    plotcur <- ggplot(big.df, aes(x = factor(Produkt), y = Bestand_Einheit, fill = factor(Produkt))) +
      theme(plot.title = element_text(hjust = 0.5)) # to center title
  } else {
    plotcur <- ggplot(big.df, aes(x = factor(Produkt), y = Bestand_Einheit)) +
      theme(plot.title = element_text(hjust = 0.5)) # to center title
  }
  # horizontal or vertical
  if (horiz == TRUE) {
    plotcur + 
      geom_col() + 
      labs(title = "Aktueller Warenbestand", x= "Produkte", y = "Warenbestand in Kilo") + 
      # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
      coord_flip() +
      guides(fill = FALSE) 
  } else {
    plotcur + 
      geom_col() + 
      labs(title = "Aktueller Warenbestand", x= "Produkte", y = "Warenbestand in Kilo") + 
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
      guides(fill = FALSE) +
      coord_cartesian(ylim = c(0,max(big.df$Bestand_Einheit))) 
  }
}
#' @title plot function for foodcoop' storage 
#' @description works with ggplot
#' @export

plotgg <- function(
  data
) {
  
  pplot <- ggplot(data, aes(x = Tag, y = Bestand.Einheit, fill = Produkt)) +
    geom_line(data = data, aes(color = Produkt)) + 
    scale_x_date() + 
    ylab("Lagerbestand")
  return(pplot)
}
list.of.prod <- function(table) {
  dif.products <- levels(table$Produkt)
  return(lapply(dif.products, '['))
}


empty.plot <- function(product) {
  plot(c(0,0,0,0), c(4,3,2,1), type="o", xlab="", ylab="", xlim=c(3,6), col=1, yaxp=c(1, 4, 3), las=1, axes=FALSE)
  legend("center", paste(product, "manuell überprüfen"))
}
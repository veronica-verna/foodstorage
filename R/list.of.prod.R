list.of.prod <- function(table) {
  dif.products <- levels(table$Produkt)
  return(lapply(dif.products, '['))
}


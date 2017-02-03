## a function for testing fun_reg, prod.df.reg and prepare
test.functions <- function(fun, products = levels(kornumsatz$Produkt)) {
  # create list if fun works or not
  list.of.works <- vector("list", length(products))
  names(list.of.works) <- products
  for (i in 1:length(products)) {
    list.of.works[i] <- try(fun(products[i], test = TRUE))
  }
  # filter only errors and warnings
  list.of.errors <- vector("list", 0)
  for (i in 1:length(list.of.works)) {
    if (list.of.works[[i]] != "yes") {
      list.of.errors[[length(list.of.errors)+1]] <- list.of.works[i]
    }  
  }
  if (length(list.of.errors) == 0) return("everything works")
  return(list.of.errors)
}

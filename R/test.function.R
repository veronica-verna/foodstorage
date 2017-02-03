## a function for testing fun_reg, prod.df.reg and prepare
test.function <- function(fun, products = levels(kornumsatz$Produkt)) {
  # how much products do we wanna test?
  len <- length(products)
  percent <- 100/len
  
  # create list if fun works or not & filter only errors and warnings
  list.of.works <- vector("list", length(products))
  names(list.of.works) <- products
  list.of.errors <- vector("list", 0)
  for (i in 1:length(products)) {
    
    list.of.works[i] <- try(fun(products[i], test = TRUE))
    if (list.of.works[[i]] != "yes") {
      list.of.errors[[length(list.of.errors)+1]] <- list.of.works[i]
    }
    print(paste(round(percent*i, 2), "%", sep=" "))
  }
  
  if (length(list.of.errors) == 0) return("everything works")
  return(list.of.errors)
}

## test specific for prod.df.reg
test.prod.df.reg <- function(products = levels(kornumsatz$Produkt), nec.dates = 10) {
  # how much products do we wanna test?
  len <- length(products)
  percent <- 100/len
  
  # create list if fun works or not & filter only errors and warnings
  list.of.works <- vector("list", length(products))
  names(list.of.works) <- products
  list.of.errors <- vector("list", 0)
  for (i in 1:length(products)) {
    
    list.of.works[i] <- try(prod.df.reg(products[i], nec.dates = nec.dates, test = TRUE))
    if (list.of.works[[i]] != "yes") {
      list.of.errors[[length(list.of.errors)+1]] <- list.of.works[i]
    }
    print(paste(round(percent*i, 2), "%", sep=" "))
  }
  
  if (length(list.of.errors) == 0) return("everything works")
  return(list.of.errors)
}
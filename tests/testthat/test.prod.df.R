context("How dataframe looks like which is needed for regression")
data("kornumsatz")
kornumsatz <- startup.settings(kornumsatz)
alle <- levels(kornumsatz$Produkt)

## test prod.df.reg
works <- data.frame(Produkt = alle, works = character(length(alle)))
for (i in 1:length(alle)) {
  works[i,2] <- "yes"
  if (is.list(prod.df.reg(alle[i])) == FALSE) next
}


for (i in 1:length(alle)) {
  print(alle[i])
  if (is.list(prod.df.reg(alle[i])) == FALSE) next
}

list.of.works <- vector("list", length(alle))
names(list.of.works) <- alle
for (i in 1:length(alle)) {
  if (try(prod.df.reg(alle[i], works = TRUE)) == "yes") {
    list.of.works <- list.of.works[-i]
  }
  list.of.works[i] <- try(prod.df.reg(alle[i], works = TRUE))
}
list.of.works[[which()]]


class(list.of.works[[]])
list.of.works[which(class(list.of.works) == "character")]
sapply(list.of.works, "[[", is.character())
### exception handling
inputs = list(1, 2, 4, -5, 'oops', 0, 10)

(for(input in inputs) {
  print(paste("log of", input, "=", log(input)))
}
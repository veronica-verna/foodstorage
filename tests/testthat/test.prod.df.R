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



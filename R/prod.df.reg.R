### function to generate the dataframe you need for calculating the regression ####

prod.df.reg <- function (product, from = "", to = "", more.than = 15, nec.dates = 10, refill.percent = 0.7, data.num.percent = 0.2, test = FALSE) {
  
  prod_df <- prepare(name.of.product = product, "regression", from = from, to = to, more.than = more.than)
  
  # get 70% of VPE
  procent70 <- prod_df$VPE[1] * refill.percent
  procent20 <- prod_df$VPE[1] * data.num.percent
  # only refill of storage
  # how many refills do we have?
  num_refill <- nrow(prod_df[prod_df$MengeDif > procent70, ])
  if (num_refill == 0) {
    # if storage isn't refilled take the highest point of storage in this time
    highest_storage <- prod_df[which(prod_df$Warenbestand == max(prod_df$Warenbestand)),]$Datum[1]
    # how much data do we have above 20%
    above_20_procent <- nrow(prod_df[prod_df$Warenbestand > procent20, ])
    if (above_20_procent > nec.dates) {
      last.refill <- highest_storage
      prod_df.reg <- prod_df[prod_df$Datum >= last.refill, ]
    } else stop("too little data for calculating a regression")
  } else {
    # normal usecase: Last refill is older than 10 days
    last.refill <- prod_df[prod_df$MengeDif > procent70, ][num_refill,]$Datum
    prod_df.reg <- prod_df[prod_df$Datum >= last.refill, ]
    
    # But take the refill before because there are too less dates since the storage was refilled the last time
    if (last.refill >= prod_df[nrow(prod_df), ]$Datum - nec.dates) {
      # Is there another refill before?
      if (num_refill > 1) {
        used.refill <- prod_df[prod_df$MengeDif > procent70, ][nrow(prod_df[prod_df$MengeDif > procent70,]) - 1,]$Datum
        prod_df.reg <- prod_df[prod_df$Datum >= used.refill & prod_df$Datum < last.refill, ]
      } else stop("There are too little data for only one existing 'refill'")
    }
  }
  #return(tail(prod_df_reg))
  if (0 %in% prod_df.reg$Warenbestand) {
    storage.is.zero <- prod_df.reg[prod_df.reg$Warenbestand == 0 & prod_df.reg$MengeDif != 0,]$Datum
    pos.dates <- seq(from = storage.is.zero[1], to = storage.is.zero[1] + more.than, by = 'day')
    dif.storage <- unique(prod_df.reg[which(prod_df.reg$Datum %in% pos.dates), ]$Warenbestand)
    if (length(dif.storage) != 1) stop("There is a mistake")
    prod_df.reg <- prod_df.reg[prod_df.reg$Datum <= storage.is.zero[1], ]
    if (length(storage.is.zero) != 1) warning("Since the last refill, more than one time storage is empty for at least 15 days")
  }
  if (test == TRUE) return("yes")
  if (exists("used.refill") == TRUE) return(list(df.big = prod_df, df = prod_df.reg, last.refill = last.refill, used.refill = used.refill))
  return(list(df.big = prod_df, df = prod_df.reg, last.refill = last.refill))
}
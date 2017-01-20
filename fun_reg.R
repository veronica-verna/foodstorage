### function for calculating a regression ###

fun_reg <- function(product,
                    type = "p",
                    col_points = "grey",
                    pch = 16,
                    main_header = "",
                    ylab = "Warenbestand in Kilo",
                    xlab = "",
                    las = 1,
                    col_line = "green",
                    col_conv = "green",
                    col_20 = "red") {
  
  # rgb(red=0.2, green=0.2, blue=0.2, alpha=0)
  prod_df <- prepare(product, "regression")
  
  # get 85% of VPE
  procent20 <- prod_df$VPE[1] * 0.7
  # only storage refill
  last.refill <- prod_df[prod_df$MengeDif > procent20, ][nrow(prod_df[prod_df$MengeDif > procent20,]),]$Datum
  prod_df.reg <- prod_df[prod_df$Datum >= last.refill, ]
  head(prod_df.reg)
  # calculate regression #
  fm_reg <- lm(Warenbestand ~ Datum, data = prod_df.reg)
  x_end <- -fm_reg$coefficients[1] / fm_reg$coefficients[2]
  date_reg <- seq(from = range(prod_df.reg$Datum)[1], to = as.Date(x_end, origin = "1970-01-01"), by = 'day')
  preds_reg <- predict(fm_reg, newdata = data.frame("Datum"=date_reg), se.fit = TRUE)
  
  # plot it
  # # how many years do we wanna plot?
  ## writing xlab automatically ##
  if (xlab == "") {
    dif.years <- as.character(levels(as.factor(year(date1st))))
    len <- length(dif.years)
    if (len == 1) {
      xlab = paste("Jahr", dif.years)
    } else {
      xlab <- paste("Jahre", dif.years[1])
      for (i in 2:len) {
        if (i == len) {
          xlab <- paste(xlab, dif.years[i], sep = " und ")
        } else {
          xlab <- paste(xlab, dif.years[i], sep = ", ")
        }
      }
    }
  }
  par(col.axis = "black")
  if (main_header == "") {
    plot(prod_df$Datum, 
         prod_df$Warenbestand,
         type = type, 
         pch = pch,
         col = col_points,
         xaxt = "n",
         xlab = xlab, 
         ylab = ylab, 
         las = las, 
         ylim = c(0, range(prod_df$Warenbestand)[2] + 3), 
         xlim = c(range(prod_df$Datum)[1], x_end),
         main = paste("Warenbestand von", product))
    warning("Your plot will look better if you fill 'main_header'")
  } else {
    plot(prod_df$Datum, 
         prod_df$Warenbestand,
         type = type, 
         pch = pch,
         col = col_points,
         xaxt = "n",
         xlab = xlab, 
         ylab = ylab, 
         las = las, 
         ylim = c(0, range(prod_df$Warenbestand)[2] + 3), 
         xlim = c(range(prod_df$Datum)[1], x_end),
         main = paste("Warenbestand von", main_header))
  }
  lines(x = date_reg, y = preds_reg$fit, col = col_line)
  lines(x = date_reg, y = preds_reg$fit + 2 * preds_reg$se.fit, lty = 2, col = col_conv)
  lines(x = date_reg, y = preds_reg$fit - 2 * preds_reg$se.fit, lty = 2, col = col_conv)
  x_20procent <- as.Date((0.2 * prod_df.reg$Warenbestand[1] - fm_reg$coefficients[1]) / fm_reg$coefficients[2], origin = "1970-01-01")
  abline(v = x_20procent, lty=3, col = col_20)
  par(col.axis = col_20)
  axis(side = 1, at = x_20procent, labels = conv.date(x_20procent), col.ticks = col_20)
  abline(h = 0.2 * prod_df.reg$Warenbestand[1], lty = 3, col = "black")
  text(x = prod_df.reg$Datum[15], y = 0.2 * prod_df.reg$Warenbestand[1] + 3, labels = "20% der letzten Bestellung")
  par(col.axis = "black")
  
  # write/draw x-axis
  start_1st_date <- min(prod_df$Datum)
  day(start_1st_date) <- 01
  if (month(start_1st_date) == 12) {
    year(start_1st_date) <- year(start_1st_date) + 1
    month(start_1st_date) <- 1
    } else { month(start_1st_date) <- month(start_1st_date) + 1
    }
  end_date <- as.Date(x_end, origin = "1970-01-01")
  date1st <- seq(from = as.Date(start_1st_date), to = end_date, by = '1 month')
  # do the writing, but first check where is x_20procent!
  day_in_num <- c(as.numeric(day(x_20procent)),
                  as.numeric(month(x_20procent)),
                  as.numeric(year(x_20procent)))
  pos.days <- c(1:31)
  
  if (day(x_20procent) %in% pos.days[1:15]) {
    without <- which(month(date1st) == day_in_num[2] & as.numeric(year(date1st)) == day_in_num[3])
    if (day(x_20procent) %in% pos.days[c(14,15)]) without <- c(without, without + 1)
    }
  if (day(x_20procent) %in% pos.days[16:31]) {
    without <- which(month(date1st) == day_in_num[2] & as.numeric(year(date1st)) == day_in_num[3]) + 1
    if (day(x_20procent) %in% pos.days[16]) without <- c(without - 1, without)
  }
  axis(1, at=date1st[-without], labels=month.abb[month(as.POSIXlt(date1st[-without], format="%Y-%m-%d"))])
}
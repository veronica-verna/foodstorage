### function for calculating a regression ###

fun_reg <- function(product,
                    type = "p",
                    col_points = "grey",
                    pch = 16,
                    xlab = "Zeit",
                    ylab = "Warenbestand in Kilo",
                    las = 1,
                    col_line = "green",
                    col_conv = "green",
                    col_20 = "red") {
  
  # rgb(red=0.2, green=0.2, blue=0.2, alpha=0)
  prod_df <- prepare(product, "regression")
  
  # get 85% of VPE
  procent20 <- prod_df$VPE[1] * 0.85
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
  par(col.axis = "black")
  plot(prod_df$Datum, 
       prod_df$Warenbestand, 
       type = type, pch = pch,
       col = col_points, 
       xlab = xlab, 
       ylab = ylab, 
       las = las, 
       ylim = c(0, range(prod_df$Warenbestand)[2] + 3), 
       xlim = c(range(prod_df$Datum)[1], x_end))
  lines(x = date_reg, y = preds_reg$fit, col = col_line)
  lines(x = date_reg, y = preds_reg$fit + 2 * preds_reg$se.fit, lty = 2, col = col_conv)
  lines(x = date_reg, y = preds_reg$fit - 2 * preds_reg$se.fit, lty = 2, col = col_conv)
  x_20procent <- as.Date((0.2 * prod_df.reg$Warenbestand[1] - fm_reg$coefficients[1]) / fm_reg$coefficients[2], origin = "1970-01-01")
  abline(v = x_20procent, lty=3, col = col_20)
  par(col.axis = col_20)
  axis(side = 1, at = x_20procent, labels = conv.date(x_20procent), col.ticks = col_20)
  abline(h = 0.2 * prod_df.reg$Warenbestand[1], lty = 3, col = "black")
  text(x = prod_df.reg$Datum[15], y = 0.2 * prod_df.reg$Warenbestand[1] + 3, labels = "20% der letzten Bestellung")
  
}
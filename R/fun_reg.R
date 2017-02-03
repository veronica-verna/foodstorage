#' function for calculating a regression 
#' 
#' @param product Need to be a character string
#' @param from As default plot will be generated for the last year
#' @param to As default end of time serie is the date of last backup import
#' @param graphics logical. If it's true, a plot will be the result. Otherwise a dataframe containing dates
#' @param type Choose type how points shall be drawn. Visible as default. 
#' @param col_points If points are drawn, choose color here. Default is grey.
#' @param pch same as usual... As default filled points are drawn.
#' @param main_header Not necessary, but it's recommended for German language
#' @param ylab As default 'food storage in kilo' in German
#' @param xlab Can be choosen, but not necessary. It's generated automatically
#' @param las Of course numbers of y-axis are rotated about 90 degrees
#' @param col_reg Optional: Choose regression line's color
#' @param col_conv Optional: Choose color of lines of confidential interval
#' @param col_20 Optional: Choose 'run-out' line's color
#' @param col_past Optional: Choose the color for loess line
#' @param smoother At this moment there is implemented only loess
#' @param span Attributes for loess. As default it's set as 0.1
#' @param degree As default it's 1
#' @param lwd Three numbers in one vector are necessary. Order is loess-regression-conf.interval
#' @param lty Same as lwd
#' @param nec.dates How many days do you want at least for calculating a regression?
#' @param more.than Important for correct giving data. Default should be ok
#' @param test logical. If TRUE you can test fun_reg by test_function {foodstorage}
#' @return If graphics is true (default), the result will be a plot. X-axis consists of a time serie, y-axis consists of the daily food storage. If graphics is false, a list is spewed containing two dates: The second one is the date, when your storage will run out of the giving product. And the first one is the date four weeks before end.
#' 


fun_reg <- function(product,
                    from = "",
                    to = "",
                    graphics = TRUE,
                    type = "p",
                    col_points = "grey",
                    pch = 16,
                    main_header = "",
                    ylab = "Warenbestand in Kilo",
                    xlab = "",
                    las = 1,
                    col_reg = "grey",
                    col_conv = "lightgrey",
                    col_20 = "red",
                    col_past = "black",
                    smoother = "loess",
                    span = 0.1,
                    degree = 1,
                    lwd = c(2,2,1),
                    lty = c(1,1,2),
                    nec.dates = 10,
                    more.than = 15,
                    test = FALSE,
                    test_pred = FALSE) {
  
  # rgb(red=0.2, green=0.2, blue=0.2, alpha=0)
  # one warning for the same is enough
  prod_df <- suppressWarnings(prod.df.reg(product, from, to, more.than, nec.dates, 0.7, 0.2)$df.big)
  prod_df.reg <- suppressWarnings(prod.df.reg(product, from, to, more.than, nec.dates, 0.7, 0.2)$df)
  last.refill <- suppressWarnings(prod.df.reg(product, from, to, more.than, nec.dates, 0.7, 0.2)$last.refill) 
  if (length(prod.df.reg(product, from, to, more.than, nec.dates, 0.7, 0.2)) == 4) {
    used.refill <- suppressWarnings(prod.df.reg(product, from, to, more.than, nec.dates, 0.7, 0.2)$used.refill)
  }
  
  # calculate regression #
  fm_reg <- lm(Warenbestand ~ Datum, data = prod_df.reg)
  if (exists("used.refill") == TRUE) {
    newIntercept <- prod_df[prod_df$Datum == last.refill,]$Warenbestand - (fm_reg$coefficients[2] * as.numeric(ymd(last.refill)))
    fm_reg$coefficients[1] <- as.numeric(as.character(newIntercept))
  }
  x_end <- -fm_reg$coefficients[1] / fm_reg$coefficients[2]
  end_date <- as.Date(x_end, origin = "1970-01-01")
  #storage.at.end <- prod_df.reg$Warenbestand[prod_df.reg$Datum == as.character(end_date)]
  if (prod_df.reg$Warenbestand[nrow(prod_df.reg)] == 0) {
    date_reg <- seq(from = last.refill, to = end_date, by = 'day')
  } else {
    if (end_date > prod_df$Datum[nrow(prod_df)]) {
      date_reg <- seq(from = prod_df$Datum[nrow(prod_df)], to = end_date, by = 'day')
    } else {
      date_reg <- seq(from = last.refill, to = prod_df$Datum[nrow(prod_df)], by = 'day')
    }
  }
  preds_reg <- predict(fm_reg, newdata = data.frame("Datum"=date_reg), se.fit = TRUE)
  
  four_weeks <- as.Date(as.character(end_date)) %m-% months(1)
  if (graphics == FALSE) return(list(four_weeks, end_date))
  
  
  # calculate loess #
  if (smoother == "loess") {
    lo <- loess(Warenbestand ~ Tag_Nr, data = prod_df, span = span, degree = degree)
    preds_lo <- predict(lo)
  }
  
  ## which time do we wanna plot? ##
  start_1st_date <- min(prod_df$Datum)
  day(start_1st_date) <- 01
  if (month(start_1st_date) == 12) {
    year(start_1st_date) <- year(start_1st_date) + 1
    month(start_1st_date) <- 1
  } else { month(start_1st_date) <- month(start_1st_date) + 1
  }
  date1st <- seq(from = as.Date(start_1st_date), to = end_date, by = '1 month')
  
  
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
  if (test == FALSE) {
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
         ylim = c(0, range(prod_df$Warenbestand)[2]), 
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
    # at first draw loess #
    lines(x = prod_df$Datum, y = preds_lo, col = col_past, lwd = lwd[1], lty = lty[1])
    # and now the regression for the future, but only if there is a future...
    lines(x = date_reg, y = preds_reg$fit, col = col_reg, lty = lty[2], lwd = lwd[2])
    lines(x = date_reg, y = preds_reg$fit + 2 * preds_reg$se.fit, lty = lty[3], lwd = lwd[3], col = col_conv)
    lines(x = date_reg, y = preds_reg$fit - 2 * preds_reg$se.fit, lty = lty[3], lwd = lwd[3], col = col_conv)
    abline(v = four_weeks, lty=3, col = col_20)
    # make the legend
    legend("topright", 
           col = c(col_past, col_reg, col_20, col_20), 
           legend = c("Bereits geschehen", "Zukunftsprognose", "4 Wochen vor Ende", as.character(conv.date(four_weeks))), 
           lty = c(1,1,3, 0), lwd =c(2,2,2,0), pch = c(NA,NA,NA,25), pt.bg = col_20)
    
    }
    
    #par(col.axis = col_20)
    #axis(side = 1, at = four_weeks, labels = conv.date(four_weeks), col.ticks = col_20)
    # other solution: four_weeks warning instead of 20%
    # abline(h = 0.2 * prod_df.reg$Warenbestand[1], lty = 3, col = "black")
    # text(x = prod_df.reg$Datum[15], y = 0.23 * prod_df.reg$Warenbestand[1], labels = "20% der letzten Bestellung")
  
  
  
  # write/draw x-axis
  if (test == FALSE) {
    axis(1, at=date1st, labels=month.abb[month(as.POSIXlt(date1st, format="%Y-%m-%d"))])
    points(x=four_weeks, y = 0.5, pch = 25, cex = 3, col = "darkred", bg = "red")
  }
  # other way for labeling x-axis
   # do the writing, but first check where is x_20procent!
  #day_in_num <- c(as.numeric(day(four_weeks)),
   #               as.numeric(month(four_weeks)),
    #              as.numeric(year(four_weeks)))
  #pos.days <- c(1:31)
  
  #if (day(four_weeks) %in% pos.days[1:15]) {
   # without <- which(month(date1st) == day_in_num[2] & as.numeric(year(date1st)) == day_in_num[3])
    #if (day(four_weeks) %in% pos.days[c(14,15)]) without <- c(without, without + 1)
    #}
  #if (day(four_weeks) %in% pos.days[16:31]) {
   # without <- which(month(date1st) == day_in_num[2] & as.numeric(year(date1st)) == day_in_num[3]) + 1
  #  if (day(four_weeks) %in% pos.days[16]) without <- c(without - 1, without)
  #}
  #if (test == FALSE) axis(1, at=date1st[-without], labels=month.abb[month(as.POSIXlt(date1st[-without], format="%Y-%m-%d"))])
  
  ## test case
  if (test == TRUE) return("yes")
}
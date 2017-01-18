## making a regression for each product ###

kornumsatz_80 <- kornumsatz[kornumsatz$MengeKum > 0 & kornumsatz$Produkt == "Sonnenblumenkerne",]
kornumsatz_80[nrow(kornumsatz_80),]
kornumsatz_80 <- kornumsatz[kornumsatz$Produkt == "Sonnenblumenkerne" & kornumsatz$Datum >= kornumsatz_80[nrow(kornumsatz_80),]$Datum,]
range(kornumsatz_80$Datum)
Datum = seq(from=range(kornumsatz_80$Datum)[1], to=range(kornumsatz_80$Datum)[2], by="day")
sortbydays <- data.frame(Tag_Nr = 1:length(Datum), Datum = Datum)
head(sortbydays)
sortbydays <- merge(sortbydays, kornumsatz_80[,c(2,3)], by='Datum', all=T)
sortbydays$MengeKum[is.na(sortbydays$MengeKum)] <- 0
sortbydays

plot(x = Datum, 
     y = data[,3], 
     xaxt = "n", 
     yaxt = "n", 
     ylab = "", 
     xlab = "", 
     ylim = c(range(preds)[1] - 0.2, range(preds)[2] + 0.2), 
     pch = pch[1], 
     main = paste(paste(colnames(data)[3], "von"), name.of.product), 
     col = rgb(red=0.2, green=0.2, blue=0.2, alpha=0))


### new trial ###
sb.kerne <- prepare("Sonnenblumenkerne", "regression")
# only storage refill
last.refill <- sb.kerne[sb.kerne$MengeDif > 0, ][nrow(sb.kerne[sb.kerne$MengeDif > 0,]),]$Datum
sb.kerne.reg <- sb.kerne[sb.kerne$Datum >= last.refill, ]
head(sb.kerne.reg)
plot(sb.kerne.reg$Datum, sb.kerne.reg$Warenbestand, type = "p", pch = 16, col = "grey", xlab = "Zeit", ylab = "Warenbestand in Kilo", las = 1, ylim = c(0, range(sb.kerne.reg$Warenbestand)[2] + 3), xlim = c(range(sb.kerne.reg$Datum)[1], x_end))
# calculate regression #
fm <- lm(sb.kerne.reg$Warenbestand ~ sb.kerne.reg$Datum)
preds <- predict(fm, se.fit = TRUE)
head(preds)
summary(fm)
# y = mx + b --> y = -0.26 * x + 111
# --> x = 111 / 0.26
x_end <- -fm$coefficients[1] / fm$coefficients[2]
lines(x = sb.kerne.reg$Datum, y = preds$fit, col = "green")
abline(a = fm$coefficients[1], b = fm$coefficients[2], col = "red")
lines(x = sb.kerne.reg$Datum, y = preds$fit + 2 * preds$se.fit, lty = 2, col = "green")
lines(x = sb.kerne.reg$Datum, y = preds$fit - 2 * preds$se.fit, lty = 2, col = "green")
abline(h = 0.2 * sb.kerne.reg$Warenbestand[1], lty = 3, col = "black")
as.POSIXct(5000)
sequence <- seq(from = range(sb.kerne.reg$Datum)[1], to = as.Date(x_end), len = 1000)

# day numbers
Jan <- 31
Feb <- 29
Mar <- 31
Apr <- 30
May <- 31
Jun <- 30
Jul <- 11
sum <- sum(c(Jan, Feb, Mar, Apr, May, Jun, Jul))

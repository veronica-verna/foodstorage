### new trial ###
sb.kerne <- prepare("Sonnenblumenkerne", "regression")
# only storage refill
last.refill <- sb.kerne[sb.kerne$MengeDif > 0, ][nrow(sb.kerne[sb.kerne$MengeDif > 0,]),]$Datum
sb.kerne.reg <- sb.kerne[sb.kerne$Datum >= last.refill, ]
head(sb.kerne.reg)
# calculate regression #
fm <- lm(Warenbestand ~ Datum, data = sb.kerne.reg)
sequence <- seq(from = range(sb.kerne.reg$Datum)[1], to = as.Date(x_end, origin = "1970-01-01"), by = 'day')
preds <- predict(fm, newdata = data.frame("Datum"=sequence), se.fit = TRUE)
head(preds)
summary(fm)
# y = mx + b --> y = -0.26 * x + 111
# --> x = 111 / 0.26
x_end <- -fm$coefficients[1] / fm$coefficients[2]
# plot it
par(col.axis = "black")
plot(sb.kerne.reg$Datum, sb.kerne.reg$Warenbestand, type = "p", pch = 16, col = "grey", xlab = "Zeit", ylab = "Warenbestand in Kilo", las = 1, ylim = c(0, range(sb.kerne.reg$Warenbestand)[2] + 3), xlim = c(range(sb.kerne.reg$Datum)[1], x_end))
lines(x = sequence, y = preds$fit, col = "green")
lines(x = sequence, y = preds$fit + 10 * preds$se.fit, lty = 2, col = "green")
lines(x = sequence, y = preds$fit - 10 * preds$se.fit, lty = 2, col = "green")
x_20procent <- as.Date((0.2 * sb.kerne.reg$Warenbestand[1] - fm$coefficients[1]) / fm$coefficients[2], origin = "1970-01-01")
abline(v = x_20procent, lty=3, col = "red")
par(col.axis = "red")
axis(side = 1, at = x_20procent, labels = conv.date(x_20procent), col.ticks = "red")
abline(h = 0.2 * sb.kerne.reg$Warenbestand[1], lty = 3, col = "black")
text(x = sb.kerne.reg$Datum[15], y = 0.2 * sb.kerne.reg$Warenbestand[1] + 3, labels = "20% der letzten Bestellung")


### new example: Basmati braun ###
basmati.br <- prepare("Basmati.Braun", "regression")
last.refill <- basmati.br[basmati.br$MengeDif > 0, ][nrow(basmati.br[basmati.br$MengeDif > 0,]),]$Datum
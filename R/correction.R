#' @export

### function for correcting the trading stock

correction <- function(sub.df, VPE, correction = 0.05, more.than = 15) {
  if (findInterval(correction, c(0,0.2)) == 0) stop("A correction for more than twenty percent isn't realistic")
  Prozent5 <- correction * VPE
  # evaluating candidates for 'correcting operation': storage below 'correction' percent (--> probably booking error)
  candidates <- sub.df[abs(sub.df$Bestand_Einheit) < Prozent5,]
  
  # how many candidates do we have?
  candidates_dif <- unique(candidates$Bestand_Einheit)
  # only filter those who stay at the same food storage for 'more.than' 15 (default) days
  dates <- as.Date(character())
  for (i in 1:length(candidates_dif)) {
    if (nrow(candidates[candidates$Bestand_Einheit == candidates_dif[i],]) > more.than) {
      dates_new <- candidates[candidates$Bestand_Einheit == candidates_dif[i],]$Datum
      dates <- c(dates, dates_new)
    }
  }
  # and how many different candidates do we have now?
  dif.storage <- unique(candidates[which(candidates$Datum %in% dates),]$Bestand_Einheit)
  if (length(dif.storage) != 0) {
    for (i in 1:length(dif.storage)) {
      MengeKum <- candidates[which(candidates$Datum %in% dates & candidates$Bestand_Einheit == dif.storage[i]) ,]$MengeKum[1]
      
      # 4 possibilities, but result is the same
      # case: storage was refilled, but too much -> MengeKum gets smaller
      # case: storage was refilled, but too less --> MengeKum gets bigger
      # case: storage was cleared, but too much
      # case: storage was cleared, but too less
      sub.df[which(sub.df$Datum %in% dates & sub.df$Bestand_Einheit == dif.storage[i]),]$MengeKum[1] <- MengeKum - dif.storage[i]
      sub.df[which(sub.df$Datum %in% dates & sub.df$Bestand_Einheit == dif.storage[i]),]$Bestand_Einheit <- 0
    }
  }
  return(sub.df)
}
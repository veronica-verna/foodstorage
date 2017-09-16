#' @export

### function for correcting the trading stock

correction <- function(sub.df, 
                       VPE, 
                       correction = 0.05, 
                       more.than = 15,
                       test_candidates = FALSE,
                       test_equal_candidates = FALSE) {
  
  if (findInterval(correction, c(0,0.2)) == 0) stop("A correction for more than twenty percent isn't realistic")
  empty <- data.frame() # an empty data.frame for containing empty 'storages'
  Prozent5 <- correction * VPE
  sub.df <- cbind(sub.df, Prozent5)
  # evaluating candidates for 'correcting operation': storage below 'correction' percent (--> probably booking error)
  candidates <- sub.df[sub.df$Bestand_Einheit < sub.df$Prozent5,]
  if (isTRUE(test_candidates)) return(candidates) # for debugging with test_that
  
  # comparison...
  candidates_eval <- sub.df[which(sub.df$Position %in% candidates$Position)+1, ]
  if (TRUE %in% is.na(candidates_eval$Position)) {
    candidates_eval <- candidates_eval[which(!is.na(candidates_eval$Position)),] 
    empty <- candidates[nrow(candidates), ]
    candidates <- candidates[-nrow(candidates),] # clean ...
  }
  if (isTRUE(test_equal_candidates)) return(nrow(candidates) - nrow(candidates_eval))
  # date_dif <- 
}
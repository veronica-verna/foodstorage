#' @export

### function for correcting the trading stock - but hopefully it becomes unnecessary with ggplot...

correction <- function(sub.df, 
                       VPE, 
                       correction = 0.05, 
                       more.than = 15,
                       test_candidates = FALSE,
                       test_equal_candidates = FALSE) {
  
  if (findInterval(correction, c(0,0.2)) != 1) stop("A correction for more than twenty percent isn't realistic")
  empty <- data.frame() # an empty data.frame for containing empty 'storages'
  Prozent5 <- correction * VPE
  sub.df <- cbind(sub.df, Prozent5)
  # evaluating candidates for 'correcting operation': storage below 'correction' percent (--> probably booking error)
  candidates <- sub.df[sub.df$Bestand_Einheit < sub.df$Prozent5,]
  if (isTRUE(test_candidates)) return(candidates) # for debugging with test_that
  
  # compare candidates with the next entry/row of sub.df and check their dates
  candidates_eval <- sub.df[which(sub.df$Position %in% candidates$Position)+1, ]
  
  # NA exists if a candidate is the last row of sub.df
  if (TRUE %in% is.na(candidates_eval$Position)) { 
    # delete row consisting of NAs
    candidates_eval <- candidates_eval[which(!is.na(candidates_eval$Position)),]
    # create a vector for candidates which are treaten as empty
    empty <- candidates[nrow(candidates), ] 
    # there is one candidate less...
    candidates <- candidates[-nrow(candidates),]
  }
  # check if candidates and candidates to evaluate have same number of rows
  if (isTRUE(test_equal_candidates)) return(nrow(candidates) - nrow(candidates_eval))
  
  
  # now let's check the candidates...
  dates_dif <- candidates_eval$Datum - candidates$Datum
}
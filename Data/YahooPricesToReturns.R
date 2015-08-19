YahooPricesToReturns = function(series) {
  mycols = grep('Adj.Close', colnames(series))
  closingprice = series[,mycols]
  N = nrow(closingprice)
  percentreturn = as.data.frame(closingprice[2:N,]) / as.data.frame(closingprice[1:(N-1),]) - 1
  mynames = strsplit(colnames(percentreturn), '.', fixed=TRUE)
  mynames = lapply(mynames, function(x) return(paste0(x[1], ".PctReturn")))
  colnames(percentreturn) = mynames
  as.matrix(na.omit(percentreturn))
}

#YahooPricesToReturns is code written by the professor, Dr. James Scott for the Predictive Modeling course.


countsToPctChng = function(series) {
  Narcotics.Change = rep(NA,36)
  for (i in 1:35) {
    percChange = (series$Narcotics.Cases[i+1] - series$Narcotics.Cases[i])/series$Narcotics.Cases[i]
    Narcotics.Change[i+1] = percChange
  }
  return Narcotics.Change
}


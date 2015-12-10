#My Scripts

#function library

to.response <- function(yhat.probabilities, threshold, n_class = 2) {
  yhat = NULL
  if (n_class == 2) {
    print('binary')
    for (prob in yhat.probabilities) {
      if (prob >= threshold) {
        yhat = c(yhat, 1)
      } else if (prob < threshold) {
        yhat = c(yhat, 0)
      } else {
        print('Error')
        yhat = c(yhat, NaN)
      }
    }
    return(yhat)
  } else if (n_class == 3) {
    print('multi-class')
    for (i in 1:length(yhat.probabilities[,1])) {
      if (yhat.probabilities[i,2] >= threshold) {
        yhat = c(yhat, 'functional needs repair')
      } else if (yhat.probabilities[i,1] > yhat.probabilities[i,3]) {
        yhat = c(yhat, 'functional')
      } else if (yhat.probabilities[i,3] > yhat.probabilities[i,1]) {
        yhat = c(yhat, 'non functional')
      } else {
        print('Error')
        yhat = c(yhat, NaN)
      }
    }
    return(yhat)
  }
}

accuracy_score <- function(yhat, actual)
{
  accuracy = sum(yhat == actual)/length(yhat)
  return(accuracy)
}



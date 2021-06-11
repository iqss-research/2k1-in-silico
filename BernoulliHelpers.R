bernDraws <- function(piParam, nTrials){
  
  random <- runif(nTrials) # n i.i.d. uniform draws
  outcome <- ifelse(random <= piParam, 1, 0) # how many < pi
  
  # cat(outcome, "\n")
  
  return(outcome)
}

bernMLE <- function(outcome, intervals = 20){
  
  nVal <- length(outcome) # Turn number of successes into K
  nSuccesses <- sum(outcome)
  
  testPiParam <- (1:intervals)/intervals
  probOutcomeGivenPi <- log((testPiParam^(nSuccesses))*((1-testPiParam)^(nVal - nSuccesses)))
  
  return <- data.frame(Pi = testPiParam, Likelihood = probOutcomeGivenPi)
  
}


bernDataPrintHelper <- function(bernData, printLength = 25){
  
  lapply(bernData[1:min(printLength, length(bernData))], function(a){
    message(a)
    Sys.sleep(0.00001)
  })
  if(length(bernData) > printLength){message("...")}
  
}
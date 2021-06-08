
##############################################
# under the hood
##############################################

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
  probOutcomeGivenPi <- (testPiParam^(nSuccesses))*((1-testPiParam)^(nVal - nSuccesses))
    
  return <- data.frame(Pi = testPiParam, Likelihood = probOutcomeGivenPi)

}



##############################################
#try it out
##############################################

# outcome <- bernDraws(piParam = .7, nTrials = 20)
# 
# 
# likelihoodDB <- bernMLE(outcome = outcome)
# 
# plot(x = likelihoodDB$pi, y = likelihoodDB$likelihood, type = "l" )

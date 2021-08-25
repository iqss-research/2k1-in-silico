source("preamble.R")


yTildeCreator <- function(paramHat, #\hat{\gamma}
                          paramVCov, #\hat{V}(\hat{\gamma})
                          model, # draws function - takes params, returns y
                          nSimDraws,
                          xRow = 1){ #X_c
  
  #get lots of parameters
  paramTilde <- rmvnorm(nSimDraws, paramHat, paramVCov)
  
  # \tilde{y}_c 
  yTilde <- sapply(1:nSimDraws, function(a){model(paramTilde[a,], 1, xRow)})
  
}





params <- 2
outcome <- poisDraws(params, 200)

MLEVals <- MLEstimator(outcome, poisChartDomain, poisLikelihoodFun)




test <- yTildeCreator(paramHat = MLEVals$paramHat,
                      paramVCov = MLEVals$paramVCov,
                      model = poisDraws,
                      nSimDraws = 1000,
                      xRow = 2)



simHist(test)

QOITables(test, "probGrt")

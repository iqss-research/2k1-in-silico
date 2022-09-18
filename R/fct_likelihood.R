

############################################################
# Functions for MLE estimation
############################################################


# TODO: separate call to optim and quadratic approx computation
likelihoodEstimateFun <- function(chartDomain, likelihoodFun,
                                  margNum, outcome,nParams, xVals, optimMethod){

  if(length(margNum) == 0){margNum <- 1}
  testParams <- rep(0.01, nParams)
  # calls to optim, with error handling
  optimizer <- tryCatch(
    {stats::optim(par = testParams,
                  likelihoodFun, hessian = T, control = list(fnscale = -1),
                  outcome = outcome, xVals = xVals, method = optimMethod)},
    error = function(e){
      tryCatch({
        optim(par = rep(.25, length(testParams)),
              likelihoodFun, hessian = T, control = list(fnscale = -1),
              outcome = outcome, xVals = xVals, method = optimMethod )},
        error = function(e){return(NULL)})
    })
  if(is.null(optimizer)){return(NULL)}
  # unpack optim results into what we want
  paramHatRaw <- optimizer$par

  # Is the Hessian invertible?
  if(det(-1*optimizer$hessian) != 0){
    paramVCov <-  solve(-1*optimizer$hessian)
    paramSE <- if(all(diag(paramVCov) >0)){diag(paramVCov) %>%  sqrt()}
  } else {
    paramVCov <- NA
    paramSE <- NA
  }

  paramHat <- c()
  paramVals <- NA
  for(j in 1:nParams){
    dmn <- chartDomain[[j]]
    pValsTmp <- seq(dmn$from, dmn$to, dmn$by)
    if(j == margNum){paramVals <- pValsTmp}
    paramHat[j] <- pValsTmp[which.min(abs(pValsTmp - paramHatRaw[j]))]
  }
  fixValues <- paramHat
  chartDomainSmall <- sapply(1:nParams, function(i){
    if(i == margNum){paramVals} else {rep(fixValues[i], length(paramVals))}
  })

  LLNew <- generalMleFun(chartDomainSmall, likelihoodFun, outcome = outcome, xVals = xVals) %>%
    dplyr::select(LogLikelihood)

  paramHatMatrixSmall <-  matrix(rep(paramHatRaw, nrow(chartDomainSmall)), ncol = ncol(chartDomainSmall), byrow = T)
  diffMatSmall <- (chartDomainSmall %>%  as.matrix() )- paramHatMatrixSmall

  QApproxNew <- c()
  for(i in 1:nrow(diffMatSmall)){

    tmpVec <- diffMatSmall[i,]
    QApproxNew <- c(QApproxNew, .5*(t(tmpVec) %*% optimizer$hessian %*%  tmpVec))

  }
  QApproxNew <- QApproxNew  + likelihoodFun(paramHatRaw, outcome = outcome, xVals = xVals)

  list(data = data.frame(param = chartDomainSmall[,margNum],LogLikelihood = LLNew, QuadraticApprox= QApproxNew),
       paramHat = paramHat,
       paramSE = paramSE,
       paramVCov = paramVCov)
}

generalMleFun <- function(chartDomain, likelihoodFun, outcome, xVals){

  LogLikelihood <- c()

  for(i in 1:nrow(chartDomain)){
    testParam <- chartDomain[i,] %>%  t() %>% as.vector()
    LogLikelihood <- c(LogLikelihood,
                       likelihoodFun(testParam =testParam, outcome = outcome, xVals = xVals))
  }

  return(cbind(param = chartDomain, as.data.frame(LogLikelihood)))
}

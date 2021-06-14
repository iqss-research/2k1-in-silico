in_silence <- function(...)
{
  mc <- match.call()[-1]
  a <- capture.output(
    tryCatch(
      suppressMessages(suppressWarnings(
        eval(as.list(mc)[[1]])
      )), error = function(e) ""))
}


quadraticLikelihoodApprox <- function(chartDomain, likelihoodFun, testParams, ...){
  
  # likelihoodFun(testParam, ...)
  
  optimizer <- optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)
  paramHat <- optimizer$par
  paramHessian <- optimizer$hessian
  
  data.frame(param = chartDomain, QuadraticApprox= optimizer$hessian*(chartDomain-paramHat)^2 + likelihoodFun(paramHat,...))
  
}


MLEPlot <- function(distrID, outcomeData){
  
  if(distrID == "Bernoulli"){
    return(bernPlot(outcomeData))
  } else(stop("Unknown Distribution!"))
  
}
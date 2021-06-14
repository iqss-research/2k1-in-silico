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
  in_silence({
  optimizer <- optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)
  paramHat <- optimizer$par
  paramHessian <- optimizer$hessian
  QApprox <-  optimizer$hessian*(chartDomain-paramHat)^2 + likelihoodFun(paramHat,...)
  })
  
  
  data.frame(param = chartDomain, QuadraticApprox= QApprox)
  
}


MLEPlot <- function(distrID, outcomeData){
  
  if(distrID == "Bernoulli"){
    return(bernPlot(outcomeData))
  } else(stop("Unknown Distribution!"))
  
}
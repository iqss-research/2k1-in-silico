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
  

  
  
  in_silence({
  result <- try({
    # optimizer <- optim(par = .6, bernLikelihoodFun, hessian = TRUE, control = list(fnscale = -1), nSuccesses = nSuccesses, nObs = nObs)
    optimizer <- optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)
  paramHat <- optimizer$par
  paramHessian <- optimizer$hessian
  paramSE <- solve(-1*optimizer$hessian) %>%  sqrt()
  QApprox <-  optimizer$hessian*(chartDomain-paramHat)^2 + likelihoodFun(paramHat,...)
  
  list(data = data.frame(param = chartDomain, QuadraticApprox= QApprox), paramHat = paramHat, paramSE = paramSE)
  })
  if (!inherits(result, "try-error")){
    ret <- result
  } else {
    ret <- list(data = data.frame(param = chartDomain, QuadraticApprox= NA), paramHat = NA, paramSE = NA)
  }
  
  
  })
  
  return(ret)
  
}

distrPlot <- function(distrID, param){
  
  if(distrID == "Bernoulli"){
    return(bernPlotDistr(param))
  } else(stop("Unknown Distribution!"))
  
  
}


MLEPlot <- function(distrID, outcomeData){
  
  if(distrID == "Bernoulli"){
    return(bernPlotMLE(outcomeData))
  } else(stop("Unknown Distribution!"))
  
}


latexSwitcher <- function(distrID, type){
  
  if(distrID == "Bernoulli"){
    return(bernMarkdown(type))
  } else(stop("Unknown Distribution!"))
  
  
}
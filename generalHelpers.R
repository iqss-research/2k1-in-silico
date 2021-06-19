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
      optimizer <- optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)
      paramHat <- optimizer$par
      paramHessian <- optimizer$hessian
      paramSE <- solve(-1*optimizer$hessian) %>%  sqrt()
      QApprox <-  optimizer$hessian*(chartDomain-paramHat)^2 + likelihoodFun(paramHat,...)
      
      list(data = data.frame(param = chartDomain, QuadraticApprox= QApprox), paramHat = paramHat, paramSE = paramSE)
    }, silent = TRUE)
    if (!inherits(result, "try-error")){
      ret <- result
    } else {
      ret <- list(data = data.frame(param = chartDomain, QuadraticApprox= NA), paramHat = NA, paramSE = NA)
    }
    
    
  })
  
  return(ret)
  
}

paramSwitcher <- function(distrID){

  if(distrID == "Bernoulli"){
    return(bernSlider)
  } else if (distrID == "Stylized Normal"){
    return(styNormSlider)
  } else(stop("Unknown Distribution!"))
  
  
}

distrPlot <- function(distrID, param){
  
  if(distrID == "Bernoulli"){
    return(bernPlotDistr(param))
  } else if (distrID == "Stylized Normal"){
    return(styNormPlotDistr(param))
  } else(stop("Unknown Distribution!"))
  
  
}


MLEPlot <- function(distrID, outcomeData){
  
  if(distrID == "Bernoulli"){
    return(bernPlotMLE(outcomeData))
  } else if (distrID == "Stylized Normal"){
    return(styNormPlotMLE(outcomeData))
  } else(stop("Unknown Distribution!"))
  
}


latexSwitcher <- function(distrID, type){
  
  if(distrID == "Bernoulli"){
    return(bernLatex(type))
  } else if (distrID == "Stylized Normal"){
    return(styNormLatex(type))
  } else(stop("Unknown Distribution!"))
  
  
}



dataPrintSwitcher <- function(distrID, header, data, printLength){
  
  if(distrID == "Bernoulli"){
    return(bernDataPrintHelper(header, data, 200))
  } else if (distrID == "Stylized Normal"){
    return(styNormDataPrintHelper(header, data, 30))
  } else(stop("Unknown Distribution!"))

  
}

drawSwitcher <- function(distrID, param, nObs){
  if(distrID == "Bernoulli"){
    return(bernDraws(param, nObs))
  } else if (distrID == "Stylized Normal"){
    return(styNormDraws(param, nObs))
  } else(stop("Unknown Distribution!"))
  
  
}


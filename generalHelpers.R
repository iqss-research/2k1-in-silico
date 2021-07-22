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
      QApprox <-  optimizer$hessian*(chartDomain-paramHat)^2/2 + likelihoodFun(paramHat,...)
      
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

generalMleFun <- function(outcome, chartDomain, LikelihoodFun){
  probOutcome <- sapply(X = chartDomain,FUN =  function(a) LikelihoodFun(testParam = a, outcome = outcome))
  
  return <- data.frame(param = chartDomain, LogLikelihood = probOutcome)
}


MLEPlotter <- function(outcome, chartDomain, LikelihoodFun, paramName = ""){
  
  xAxisName <- paste0("Parameter ", paramName)
  
  likelihoodDB <- generalMleFun(outcome,chartDomain, LikelihoodFun)
  chartLen <- length(chartDomain)
  
  qApprox <- quadraticLikelihoodApprox(
    likelihoodFun = LikelihoodFun,
    chartDomain = chartDomain, testParams = .5, outcome = outcome)
  likelihoodDB <- likelihoodDB %>%  left_join(qApprox$data, by = c("param" = "param") ) 
  
  paramHat <- likelihoodDB$param[which(likelihoodDB$LogLikelihood == max(likelihoodDB$LogLikelihood))]
  labelLLY <- max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)][.1*chartLen])/max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)])), .15)
  
  
  ret <- ggplot() + 
    geom_line(data = likelihoodDB, mapping =  aes(x = param, y = LogLikelihood), color = "steelblue", size = 1) + 
    theme_minimal() +
    xlab(xAxisName) +
    theme(text = element_text(family = "sans"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    )
  
  if(any(!is.na(likelihoodDB$QuadraticApprox))){
    
    labelQAY <- max(abs(likelihoodDB$QuadraticApprox[.1*chartLen])/max(c(abs(likelihoodDB$QuadraticApprox), abs(likelihoodDB$LogLikelihood))), .15)
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood (MLE: ", sprintf("%0.2f", paramHat), ")"),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation (SE: ", sprintf("%0.2f", qApprox$paramSE), ")"),
                               x=0.05,  y=1-labelQAY, hjust=0,
                               gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
    
    ret <- ret + geom_line(data = likelihoodDB, mapping =  aes(x = param, y = QuadraticApprox), color = "firebrick4", size = 1)  + annotation_custom(grob1)+ annotation_custom(grob2)
    
  } else {
    
    labelQAY <- .95
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood - MLE ", sprintf("%0.2f", paramHat)),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation Not Found"),
                               x=0.05,  y=1-labelQAY, hjust=0, gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
    ret <- ret + annotation_custom(grob1)+ annotation_custom(grob2)
  }
  
  ret
  
  
}


decPrintHelper <- function(header, data, printLength){
  
  if(length(data) > printLength){truncData <- data[1:printLength]}
  else{truncData <- data}
  charData <- lapply(truncData, function(s){sprintf("%0.1f",s)}) %>%  unlist()
  
  printstr <- paste(c(charData), collapse = ", ")
  printstr <- paste(header, printstr, sep = "")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
}


intPrintHelper <- function(header, data, printLength = 25){
  
  printstr <- paste(c(header, data), sep = " ")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
}

##########################################################
# Switchers
# Choose between distr-specific functions
##########################################################

paramSwitcher <- function(distrID){

  if(distrID == "Bernoulli"){
    return(bernSlider)
  } else if (distrID == "Stylized Normal"){
    return(styNormSlider)
  } else if (distrID == "Poisson"){
    return(poisSlider)
  } else if (distrID == "Exponential"){
    return(expSlider)
  } else if (distrID == "Log-Normal"){
    return(logNormSlider)
  } else(stop("Unknown Distribution!"))
  
  
}

distrPlot <- function(distrID, param){
  
  if(distrID == "Bernoulli"){
    return(bernPlotDistr(param))
  } else if (distrID == "Stylized Normal"){
    return(styNormPlotDistr(param))
  } else if (distrID == "Poisson"){
    return(poisPlotDistr(param))
  } else if (distrID == "Exponential"){
    return(expPlotDistr(param))
  } else if (distrID == "Log-Normal"){
    return(logNormPlotDistr(param))
  } else(stop("Unknown Distribution!"))
  
  
}


MLEPlot <- function(distrID, outcomeData){
  
  if(distrID == "Bernoulli"){
    return(MLEPlotter(outcomeData, bernChartDomain, bernLikelihoodFun, "Pi" ))
  } else if (distrID == "Stylized Normal"){
    return(MLEPlotter(outcomeData, styNormChartDomain, styNormLikelihoodFun, "Beta"))
  } else if (distrID == "Poisson"){
    return(MLEPlotter(outcomeData, poisChartDomain, poisLikelihoodFun, "Lambda"))
  } else if (distrID == "Exponential"){
    return(MLEPlotter(outcomeData, expChartDomain, expLikelihoodFun, "Lambda" ))
  } else if (distrID == "Log-Normal"){
    return(MLEPlotter(outcomeData, logNormChartDomain, logNormLikelihoodFun, "Beta"))
  } else(stop("Unknown Distribution!"))
  
}




dataPrintSwitcher <- function(distrID, header, data, printLength){
  
  if(distrID == "Bernoulli"){
    return(intPrintHelper(header, data, 200))
  } else if (distrID == "Stylized Normal"){
    return(decPrintHelper(header, data, 200))
  } else if (distrID == "Poisson"){
    return(intPrintHelper(header, data, 200))
  } else if (distrID == "Exponential"){
    return(decPrintHelper(header, data, 200))
  } else if (distrID == "Log-Normal"){
    return(decPrintHelper(header, data, 200))
  } else(stop("Unknown Distribution!"))

  
}

drawSwitcher <- function(distrID, param, nObs){
  if(distrID == "Bernoulli"){
    return(bernDraws(param, nObs))
  } else if (distrID == "Stylized Normal"){
    return(styNormDraws(param, nObs))
  } else if (distrID == "Poisson"){
    return(poisDraws(param, nObs))
  } else if (distrID == "Exponential"){
    return(expDraws(param, nObs))
  } else if (distrID == "Log-Normal"){
    return(logNormDraws(param, nObs))
  } else(stop("Unknown Distribution!"))
  
  
}



latexSwitcher <- function(distrID, type){
  
  if(distrID == "Bernoulli"){
    return(bernLatex(type))
  } else if (distrID == "Stylized Normal"){
    return(styNormLatex(type))
  } else if (distrID == "Poisson"){
    return(poisLatex(type))
  } else if (distrID == "Exponential"){
    return(expLatex(type))
  } else if (distrID == "Log-Normal"){
    return(logNormLatex(type))
  } else(stop("Unknown Distribution!"))
  
  
}



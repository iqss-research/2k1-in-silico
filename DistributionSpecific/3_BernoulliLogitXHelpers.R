bernLogitXSlider <- manyParamSliderMaker(minVal = -2, maxVal = 2, startVals = c(1,-1,0), stepVal = .25)

bernLogitXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  1/(1 + exp(- as.numeric(xVals %*% c(p))))
}



bernLogitXPlotDistr <- function(param){
  
  if(is.null(param)){ret <- element_blank()}
  else{

    analyticalDistr <- data.frame(
      drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Successes (1)", "Failures (0)")),
      prob = c(param, 1-param)
    )
    
    ret <- binaryDistrPlotter(analyticalDistr, param, "\\pi", roundDigits = 2)
    
  }
  ret
}


bernLogitXDraws <- bernDraws

bernLogitXLikelihoodFun <- function(testParam, outcome){
  
  nParams <- length(testParam)
  indepVars <- indepVarsBase[1:length(outcome),1:nParams]
  
  paramTransform <- 1/(1 + exp(-(indepVars %*% testParam)))
  
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  ret <- sum(log((paramTransform^(nSuccesses))*((1-paramTransform)^(nObs - nSuccesses))))
  
  if(ret < -9e20){ret <- -9e20}
  
  ret
  
  
}


singleChartDomain <- seq(from = -2, to = 2, by = .05 )
bernLogitXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)


bernLogitXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Bernoulli",
    pdfTex = " P(y_i|\\beta) = \\pi_i^{y_i}(1-\\pi_i)^{(1-y_i)}",
    pdfAddendum = 2,
    modelDistTex = "\\text{Bernoulli}(\\pi_i)",
    modelParamTex = "\\pi_i =  1/(1 + \\exp(-X_i\\beta))  ",
    likelihoodTex = "L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\left ( \\frac{1}{1 + \\exp(-X_i\\beta)}\\right)^{y_i} \\cdot \\left(  \\frac{{\\exp(-X_i\\beta)}}{{1 + \\exp(-X_i\\beta)}} \\right )^{{(1-y_i)}}",
    logLikelihoodTex = "\\ln[L(\\beta|y)] \\dot{=}   -\\sum_{i=1}^{n} \\ln(1+ \\exp(-X_i\\beta[1-2y_i]))",
    smallLik = 2, 
    smallLL = 1,
    ...
  )
  
}

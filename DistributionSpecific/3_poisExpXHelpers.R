poisExpXSlider <- manyParamSliderMaker(minVal = -1, maxVal = 1, startVals = c(.3,.1,-.3), stepVal = .1)

poisExpXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  exp(as.numeric(xVals %*% c(p)))
}


poisExpXPlotDistr <- function(param){
  
  if(is.null(param)){ret <- element_blank()}
  else{

    analyticalDistr <- data.frame(drawVal = seq(0,30,2))
    analyticalDistr <- analyticalDistr %>%  mutate(prob = (param^drawVal)*exp(-param)/(factorial(drawVal)))
    
    ret <- continuousDistrPlotter(analyticalDistr, param, '\\lambda', roundDigits = 2, arrow = FALSE, discreteOutput = TRUE, ylims = c(0, .5))
  }
  
  ret
}


poisExpXDraws <- function(params, nObs){
  # takes a 1xn vector of params, returns 1xn draws 
  sapply(params, function(a){poisDraws(a,1)})
}

poisExpXLikelihoodFun <- function(testParam, outcome, xVals){
  
  nParams <- length(testParam)
  indepVars <- xVals[1:length(outcome),1:nParams]
  
  paramTransform <- exp(indepVars %*% testParam)
  sum(outcome * log(paramTransform) - paramTransform)
  
}


singleChartDomain <- seq(from = -2, to = 2, by = .05 )
poisExpXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)



poisExpXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Poisson",
    pdfTex = "P(y|\\lambda) =  \\frac{\\lambda^y  \\exp(-\\lambda)}{y!}  ",
    pdfAddendum = 2,
    modelDistTex = "\\text{Poisson}(\\lambda_i)",
    modelParamTex = "\\lambda_i =  \\text{exp}(X_i \\beta)  ",
    likelihoodTex = "L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\text{exp}(X_i\\beta)^{y_i}  \\text{exp}(-\\text{exp}(X_i\\beta))}{y_i!}",
    logLikelihoodTex = " \\ln[ L(\\beta|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  X_i\\beta  - \\text{exp}(X_i\\beta) \\right)",
    smallLik = T,
    ...
  )
  
}

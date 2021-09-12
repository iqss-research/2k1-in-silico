logNormXSlider <- manyParamSliderMaker(minVal = -1, maxVal = 2, startVals = c(1,-1,.5), stepVal = .25)

logNormXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  as.numeric(xVals %*% c(p))
}



logNormXPlotDistr <- function(param){
  
  if(is.null(param)){ret <- element_blank()}
  else{
    
    analyticalDistr <- data.frame(drawVal = seq(0.01,10,.01))
    analyticalDistr <- analyticalDistr %>%  mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (log(drawVal) - param)^2))
    
    ret <- continuousDistrPlotter(analyticalDistr, param, "\\mu", annotationX = param)
    
  }
  
  ret
}




logNormXDraws <- function(params, nObs){
  # takes a 1xn vector of params, returns 1xn draws 
  sapply(params, function(a){logNormDraws(a,1)})
}

logNormXLikelihoodFun <- function(testParam, outcome, xVals){
  
  nParams <- length(testParam)
  indepVars <- xVals[1:length(outcome),1:nParams]
  (-1/2)*sum((log(outcome)-(indepVars %*% testParam))^2)
  
}


singleChartDomain <- seq(from = -5, to = 5, by = .05 )
logNormXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)




logNormXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Log  Normal",
    pdfTex = " P(y|\\beta) = (y\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y) - \\beta)^2}{2} \\right) ",
    pdfAddendum = 2,
    modelDistTex = " \\text{LogNormal}(\\mu_i) ",
    modelParamTex = " \\mu_i = X_i\\beta ",
    likelihoodTex = " L(\\beta|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(\\ln(y_i) - X_i\\beta)^2}{2} \\right)",
    logLikelihoodTex = " \\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (\\ln(y_i) - X_i\\beta)^2",
    smallLik = T,
    ...
  )
}
expExpXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  paramTransform <- exp(-as.numeric(xVals %*% c(p)))
}


expExpXPlotDistr <- function(param, domain){
  
  if(is.null(param)){ret <- element_blank()}
  else{

    analyticalDistr <- data.frame(drawVal = 0:500/100) %>%  mutate(prob = param*exp(-drawVal*param))
    ret <- continuousDistrPlotter(
      analyticalDistr, param, '\\lambda',roundDigits = 2, arrow = FALSE, ylims = c(0, 1.5))   
  }
  
  ret
}

expExpXDraws <- function(params, nObs){
  # takes a 1xn vector of params, returns 1xn draws 
  sapply(params, function(a){expDraws(a,1)})
}

expExpXLikelihoodFun <- function(testParam, outcome, xVals){
  
  nParams <- length(testParam)
  indepVars <- xVals[1:length(outcome),1:nParams]
  
  paramTransform <- exp(-indepVars %*% testParam)
  sum(log(paramTransform) - paramTransform*outcome)
}


singleChartDomain <- seq(from = -2, to = 2, by = .01 )
expExpXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)


expExpXLatex <- function(type,...){
  distrLatexFunction(
    type = type, 
    modelName = "Exponential",
    pdfTex = "P(y|\\lambda) =  \\lambda \\exp(-\\lambda y) ",
    pdfAddendum = 2,
    modelDistTex = "\\text{Exponential}(\\lambda_i)",
    modelParamTex = "\\lambda_i =  \\text{exp}(-\\beta) ",
    likelihoodTex = " L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-X_i\\beta) \\exp(-\\text{exp}(-X_i\\beta) y_i) ",
    logLikelihoodTex = 
      "\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (X_i\\beta + \\text{exp}(-X_i\\beta) y_i)",
    ...
  )
  
}

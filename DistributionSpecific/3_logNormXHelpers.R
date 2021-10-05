logNormXPDF <- logNormPDF

logNormXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  as.numeric(xVals %*% c(p))
}

logNormXPlotDistr <- function(param,domain, range){
  
  if(is.null(param)){ret <- element_blank()}
  else{
    ret <- multiModelDensity(param = param, domain = domain, pdf = logNormPDF, 
                             paramVal = NA, paramTex = "\\beta", annotationX = NULL, arrow = F, annotate = F, 
                             ylims = range)
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


singleChartDomain <- list(from = -5, to = 5, by = .01 )
logNormXChartDomain <- list(
  singleChartDomain,
  singleChartDomain,
  singleChartDomain)




logNormXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Log Normal",
    pdfTex = " P(y_i|\\beta) = (y\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y_i) - \\mu_i)^2}{2} \\right) ",
    pdfAddendum = 2,
    modelDistTex = " \\text{LogNormal}(\\mu_i) ",
    modelParamTex = " \\mu_i = X_i\\beta ",
    likelihoodTex = " L(\\beta|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(\\ln(y_i) - X_i\\beta)^2}{2} \\right)",
    logLikelihoodTex = " \\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (\\ln(y_i) - X_i\\beta)^2",
    smallLik = T,
    ...
  )
}
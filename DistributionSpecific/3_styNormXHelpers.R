styNormXPDF <- styNormPDF

styNormXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  as.numeric(xVals %*% c(p))}


styNormXPlotDistr <- function(param, domain, range){
  
  if(is.null(param)){ret <- element_blank()}
  else{
    ret <- multiModelDensity(param = param, domain = domain, pdf = styNormXPDF, 
                      paramVal = NA, paramTex = "", annotationX = NULL, arrow = F, annotate = F, 
                      ylims = range)
  }
  ret
}




styNormXDraws <- function(params, nObs){
  # takes a 1xn vector of params, returns 1xn draws 
  sapply(params, function(a){styNormDraws(a,1)})
}

styNormXLikelihoodFun <- function(testParam, outcome, xVals){
  
  nParams <- length(testParam)
  indepVars <- xVals[1:length(outcome),1:nParams]
  (-1/2)*sum((outcome-(indepVars %*% testParam))^2)
  
}


singleChartDomain <- seq(from = -5, to = 5, by = .05 )
styNormXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)


styNormXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Stylized Normal",
    pdfTex = " P(y_i|\\beta) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\mu_i)^2}{2} \\right)  ",
    pdfAddendum = 2,
    modelDistTex = " f_{stn}(\\mu_i) ",
    modelParamTex = "\\mu_i = X_i \\beta ",
    likelihoodTex = " L(\\beta|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - X_i\\beta)^2}{2} \\right)",
    logLikelihoodTex = "\\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - X_i\\beta)^2",
    smallLik = T,
    ...
  )
}
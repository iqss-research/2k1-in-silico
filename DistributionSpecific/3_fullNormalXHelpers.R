fullNormXSlider <- manyParamSliderMaker(minVal = -2, maxVal = 2, startVals = c(0,0,0,1), stepVal = .25, multi = "fullNorm")


######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, sigma has to be nth parameter specified here
sigmaIndex <- 4

fullNormXParamTransform <- function(p,xVals){
  pCut <- p[1:(sigmaIndex-1)]
  
  if(length(pCut)!=length(xVals)){ return(1)}
  muParam <- as.numeric(xVals %*% c(pCut))
  
  return(matrix(c(muParam, p[sigmaIndex]), ncol = 2, byrow = F))  
}




fullNormXPlotDistr <- function(param){
  paramMu <- param[1]
  paramSigma <- param[2]
  
  
  if(is.null(param)){ret <- element_blank()}
  else{
    
    analyticalDistr <- data.frame(drawVal = seq(-3,3,.01) + paramMu) %>% 
      mutate(prob = (2*pi*paramSigma^2)^(-1/2)*
             exp(-(1/(2*paramSigma^2))* (drawVal - paramMu)^2))
    
    ret <- continuousDistrPlotter(distrDF = analyticalDistr, paramVal = paramMu, paramTex = "\\mu", annotationX = paramMu)
    
  }
  ret
}




fullNormXDraws <- function(params, nObs){
  # pass it a 2 column matrix of params
  paramMat <- matrix(params, ncol = 2)
  # takes each row of params, returns a draw from a normal with mu and sigma
  apply(X = paramMat, MARGIN = 1, function(a){rnorm(1,a[1],a[2])})
}

fullNormXLikelihoodFun <- function(testParam, outcome, xVals){
  
  pCut <- testParam[1:(sigmaIndex-1)]
  paramSigma <- testParam[sigmaIndex]
  nParams <- length(pCut)
  nObs <- length(outcome)
  indepVars <- xVals[1:nObs,1:nParams]
  paramMu <- as.numeric(indepVars %*% c(pCut))
  
  -(nObs/2)*(log(paramSigma)) - (1/(2*(paramSigma^2)))*sum((outcome-paramMu)^2)
  
}


singleChartDomain <- seq(from = -5, to = 5, by = .2 )
sigmaChartDomain <- seq(from = 0.2, to = 5, by = .2 )
# TODO: don't compute this before it's necessary
fullNormXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain,
    sigmaChartDomain)


fullNormXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = " Normal",
    pdfTex = " P(y|\\beta) = (2\\pi\\sigma^2)^{-1/2} \\text{exp} \\left( \\frac{(y - \\beta)^2}{2\\sigma^2} \\right)  ",
    pdfAddendum = 2,
    modelDistTex = " \\mathcal{N}(\\mu_i, \\sigma^2) ",
    modelParamTex = "\\mu_i = X_i \\beta ",
    likelihoodTex = " L(\\beta, \\sigma|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi\\sigma^2)^{-1/2} \\text{exp} \\left( \\frac{(y_i - X_i\\beta)^2}{2\\sigma^2} \\right)",
    logLikelihoodTex = "\\ln[ L(\\beta, \\sigma|y, X)] \\, \\dot{=}\\, -\\frac{n}{2}\\ln(\\sigma) -\\frac{1}{2\\sigma^2} \\sum_{i=1}^{n} (y_i - X_i\\beta)^2",
    smallLik = T,
    smallLL = T,
    ...
  )
}
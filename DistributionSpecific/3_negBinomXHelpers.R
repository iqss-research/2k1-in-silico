######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, sigma has to be nth parameter specified here
sigmaIndex <- 4

negBinomXParamTransform <- function(p,xVals){
  pCut <- p[1:(sigmaIndex-1)]
  
  if(length(pCut)!=length(xVals)){ return(1)}
  muParam <- as.numeric(xVals %*% c(pCut))
  
  return(matrix(c(muParam, p[sigmaIndex]), ncol = 2, byrow = F))  
}

negBinomXPDF <- function(drawVal, param){
  (2*pi*param[2]^2)^(-1/2)* exp(-(1/(2*param[2]^2))* (drawVal - param[1])^2)
}

negBinomXPlotDistr <- function(param, domain, range){
  
  if(is.null(param)){ret <- element_blank()}
  else{
    ret <- multiModelDensity(param = param, domain = domain, pdf = negBinomXPDF, 
                             paramVal = NA, paramTex = "", annotationX = NULL, arrow = F, annotate = F, 
                             ylims = range)
  }
  ret
}

negBinomXDraws <- function(params, nObs){
  # pass it a 2 column matrix of params
  paramMat <- matrix(params, ncol = 2)
  # takes each row of params, returns a draw from a normal with mu and sigma
  apply(X = paramMat, MARGIN = 1, function(a){rnorm(1,a[1],a[2])})
}

negBinomXLikelihoodFun <- function(testParam, outcome, xVals){
  
  pCut <- testParam[1:(sigmaIndex-1)]
  paramSigma <- testParam[sigmaIndex]
  nParams <- length(pCut)
  nObs <- length(outcome)
  indepVars <- xVals[1:nObs,1:nParams]
  paramMu <- as.numeric(indepVars %*% c(pCut))
  
  -(nObs/2)*(log(paramSigma)) - (1/(2*(paramSigma^2)))*sum((outcome-paramMu)^2)
  
}


singleChartDomain <- list(from = -5, to = 5, by = .01 )
sigmaChartDomain <- list(from = 0.2, to = 5, by = .01 )
negBinomXChartDomain <- 
  list(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain,
    sigmaChartDomain)


negBinomXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Negative Binomial",
    pdfTex = "P(y_i|\\lambda_i, \\sigma^2) = \\frac{\\Gamma (\\frac{-\\lambda_i}{\\sigma^2 -1} +1 )}{y_i! \\Gamma (\\frac{-\\lambda_i}{\\sigma^2 -1 } - y_i + 1)}\\cdot \\) \\( \\hspace{45px} (1-\\sigma^2)^{y_i}(\\sigma^2)^{\\frac{-\\lambda_i}{\\sigma^2 - 1} - y_i } ",
    pdfAddendum = 3,
    modelDistTex = " \\text{Neg. Binom.}(\\lambda_i, \\sigma^2)",
    modelParamTex = "\\lambda_i = \\exp(X_i\\beta)",
    likelihoodTex = " L(\\lambda_i, \\sigma^2|y, X)= k(y) \\cdot  \\prod_{i = 1}^{n}  \\frac{\\Gamma (\\frac{-\\lambda_i}{\\sigma^2 -1} +1 )}{y_i! \\Gamma (\\frac{-\\lambda_i}{\\sigma^2 - 1} - y_i + 1 )}\\cdot }\\) \\({\\small \\hspace{30px} (1-\\sigma^2)^{y_i}(\\sigma^2)^{\\frac{-\\lambda_i}{\\sigma^2 - 1}-y_i} ",
    logLikelihoodTex = "\\ln[ L(\\lambda_i, \\sigma^2|y, X)] \\, \\dot{=}\\, \\sum_{i = 1}^{n} \\bigg\\{ \\ln \\Gamma(\\frac{-\\lambda_i}{\\sigma^2 -1} + 1) - }\\) \\({ \\small \\hspace{45px} \\ln \\Gamma(\\frac{-\\lambda_i}{\\sigma^2 -1} - y_i + 1) + y_i \\ln(1-\\sigma^2) + }\\) \\({ \\hspace{45px} \\small (\\frac{\\frac{-\\lambda_i}{\\sigma^2 -1} - y_i})\\ln(\\sigma^2) - \\ln(D_i) \\bigg\\}  ",
    smallLik = T,
    smallLL = T,
    ...
  )
}
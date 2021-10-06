######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, sigma has to be nth parameter specified here
sigmaIndex <- 4

negBinomXParamTransform <- function(p,xVals){
  pCut <- p[1:(sigmaIndex-1)]
  
  if(length(pCut)!=length(xVals)){ return(1)}
  lParam <- as.numeric(exp(xVals %*% c(pCut)))
  
  return(matrix(c(lParam, p[sigmaIndex]), ncol = 2, byrow = F))  
}


negBinomXPDF <- function(drawVal, param){
  tmp <- (param[1])/(param[2]^2 - 1)
  (gamma(tmp + drawVal)/(factorial(drawVal)*gamma(tmp)))*
    ((param[2]^2-1)/(param[2]^2))^drawVal * 
    (param[2]^2)^(-1*tmp)
  
}

negBinomXPlotDistr <- function(param, domain, range){
  
  if(is.null(param)){ret <- element_blank()}
  else{
    ret <- multiModelDensity(param = param, domain = domain, pdf = negBinomXPDF, 
                             paramVal = NA, paramTex = "\\beta", annotationX = NULL, arrow = F, annotate = F, 
                             ylims = range)
  }
  ret
}

negBinomXDraws <- function(params, nObs){
  # pass it a 2 column matrix of params
  paramMat <- matrix(params, ncol = 2)
  # takes each row of params, returns a draw from a negative binomial
  apply(X = paramMat, MARGIN = 1, function(a){
    # first draw a series of lambdas from gammas with the specified parameters
    scale <- params[2]^2 -1
    shape <- params[1]/(scale)
    
    lambdas <- rgamma(n = nObs, shape = shape, scale = scale)
    sapply(lambdas, function(a) rpois(1,a))
    
  })
    
}

negBinomXLikelihoodFun <- function(testParam, outcome, xVals){
  
  pCut <- testParam[1:(sigmaIndex-1)]
  paramSigma <- testParam[sigmaIndex]
  nParams <- length(pCut)
  nObs <- length(outcome)
  indepVars <- xVals[1:nObs,1:nParams]
  paramLambda <- as.numeric(indepVars %*% c(pCut))
  
  tmp <- (paramLambda)/(paramSigma^2 - 1)
  
  # yuk 
  lgamma(tmp + outcome) - lgamma(tmp) + outcome*log(paramSigma^2-1) - log(paramSigma^2)*(outcome + tmp)
  
}


singleChartDomain <- list(from = -.5, to = 5, by = .01 )
sigmaChartDomain <- list(from = 0.2, to = 5, by = .01 )
negBinomXChartDomain <- 
  list(singleChartDomain,
       singleChartDomain,
       singleChartDomain,
       sigmaChartDomain)


negBinomXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Negative Binomial",
    pdfTex = "{\\small P(y_i|\\lambda_i, \\sigma^2) = \\frac{\\Gamma (\\frac{\\lambda_i}{\\sigma^2 -1} +y_i )}{y_i! \\Gamma (\\frac{\\lambda_i}{\\sigma^2 -1 })}\\left(\\frac{\\sigma^2 - 1}{\\sigma^2}\\right)^{y_i}(\\sigma^2)^{\\frac{-\\lambda_i}{\\sigma^2 - 1}} }",
    pdfAddendum = 3,
    modelDistTex = " \\text{Neg. Binom.}(\\lambda_i, \\sigma^2)",
    modelParamTex = "\\lambda_i = \\exp(X_i\\beta)",
    likelihoodTex = " L(\\lambda_i, \\sigma^2|y, X)= k(y) \\cdot  \\prod_{i = 1}^{n} \\frac{\\Gamma (\\frac{-\\lambda_i}{\\sigma^2 -1} +y_i )}{y_i! \\Gamma (\\frac{-\\lambda_i}{\\sigma^2 -1 })}\\left(\\frac{\\sigma^2 - 1}{\\sigma^2}\\right)^{y_i}(\\sigma^2)^{\\frac{-\\lambda_i}{\\sigma^2 - 1}}",
    logLikelihoodTex = "\\ln[ L(\\lambda_i, \\sigma^2|y, X)] \\, \\dot{=}\\, \\sum_{i = 1}^{n} \\bigg\\{ \\ln \\Gamma(\\frac{\\lambda_i}{\\sigma^2 -1} + y_i) - }\\) \\({ \\small \\hspace{45px} \\ln \\Gamma(\\frac{\\lambda_i}{\\sigma^2 -1}) + y_i \\ln(1-\\sigma^2) - }\\) \\({ \\hspace{45px} \\small \\ln(\\sigma^2)\\left( y_i + \\frac{\\lambda_i}{\\sigma^2 -1}\\right) \\bigg\\}  ",
    smallLik = T,
    smallLL = T,
    ...
  )
}
######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, sigma has to be last parameter specified here

negBinomXParamTransform <- function(p,xVals, DGP = T){
  pCut <- p[1:(length(p)-1)]
  gammaVal <- p[length(p)]
  sigmaVal <- if(DGP){gammaVal} else {sqrt(exp(gammaVal)^2 + 1)}
  
  if(length(pCut)!=length(xVals)){ return(1)}
  lParam <- as.numeric(exp(xVals %*% c(pCut)))
  
  return(matrix(c(lParam, sigmaVal), ncol = 2, byrow = F))  
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
  if(is.null(params)){return(rpois(nObs, 2))}
  if(is.null(nObs)){nObs <- 20}
  paramMat <- matrix(params, ncol = 2)
  # takes each row of params, returns a draw from a negative binomial
  apply(X = paramMat, MARGIN = 1, function(a){
    # first draw a series of lambdas from gammas with the specified parameters
    scale <- a[2]^2 -1
    shape <- a[1]/(scale)
    
    lambdas <- rgamma(n = 1, shape = shape, scale = scale)
    sapply(lambdas, function(a) rpois(1,a))
    
  })
  
}

negBinomXLikelihoodFun <- function(testParam, outcome, xVals){
  # browser()
  pCut <- testParam[1:(length(testParam)-1)]
  paramSigma <- sqrt(exp(testParam[length(testParam)])^2+1)
  nParams <- length(pCut)
  nObs <- length(outcome)
  indepVars <- xVals[1:nObs,1:nParams]
  paramLambda <- exp(as.numeric(indepVars %*% c(pCut)))
  
  tmp <- (paramLambda)/(paramSigma^2 - 1)
  
  # yuk 
  sum(sapply(1:nObs, function(i){
    lgamma(tmp[i] + outcome[i]) -
      lgamma(tmp[i]) + 
      outcome[i]*log(paramSigma^2-1) -
      log(paramSigma^2)*(outcome[i] + tmp[i])
  }))
  
}


singleChartDomain <- list(from = -3, to = 3, by = .01 )
sigmaChartDomain <- list(from = -3, to = 3, by = .01 )
negBinomXChartDomain <- function(n){
  d <- lapply(1:n, function(i){singleChartDomain})
} 


negBinomXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Negative Binomial",
    pdfTex = "{\\small P(y_i|\\lambda_i, \\sigma^2) = \\frac{\\Gamma (\\frac{\\lambda_i}{\\sigma^2 -1} +y_i )}{y_i! \\Gamma (\\frac{\\lambda_i}{\\sigma^2 -1 })}\\left(\\frac{\\sigma^2 - 1}{\\sigma^2}\\right)^{y_i}(\\sigma^2)^{\\frac{-\\lambda_i}{\\sigma^2 - 1}} }",
    pdfAddendum = 2,
    modelDistTex = " \\text{Neg. Binom.}(\\lambda_i, \\sigma^2)",
    dgpParamTex = "\\lambda_i = \\exp(X_i\\beta)",
    modelParamTex = "\\lambda_i = \\exp(X_i\\beta) \\quad \\text{and} \\quad \\sigma^2 = 1+\\exp(\\gamma)^2",
    likelihoodTex = " L(\\beta, \\gamma|y, X)= k(y) \\cdot  \\prod_{i = 1}^{n} \\frac{\\Gamma \\left( \\frac{-\\lambda_i}{\\exp(\\gamma)^2} +y_i  \\right)}{y_i! \\Gamma \\left(\\frac{-\\lambda_i}{\\exp(\\gamma)^2} \\right)}\\left(\\frac{\\exp(\\gamma)^2}{1+\\exp(\\gamma)^2}\\right)^{y_i}(1+\\exp(\\gamma)^2)^{\\frac{-\\lambda_i}{\\exp(\\gamma)^2}}",
    logLikelihoodTex = "\\ln[ L(\\beta, \\gamma|y, X)] \\, \\dot{=}\\, \\sum_{i = 1}^{n} \\bigg\\{ \\ln \\Gamma \\left( \\frac{\\lambda_i}{\\exp(\\gamma)^2} + y_i \\right) - }\\) \\({ \\small \\ln \\Gamma \\left(\\frac{\\lambda_i}{\\exp(\\gamma)^2} \\right) + y_i \\gamma - }\\) \\({ \\hspace{45px} \\small \\ln(1+\\exp(\\gamma)^2)\\left( y_i + \\frac{\\lambda_i}{\\exp(\\gamma)^2}\\right) \\bigg\\}  ",
    smallLik = T,
    smallLL = T,
    ...
  )
}
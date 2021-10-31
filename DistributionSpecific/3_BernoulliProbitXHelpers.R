bernProbitXPDF <- bernPDF

bernProbitXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  VGAM::probitlink(as.numeric(xVals %*% c(p)), inverse = T)
}

bernProbitXPlotDistr <- function(param, domain, range){
  
  if(is.null(param)){ret <- element_blank()}
  else{
    analyticalDistr <- data.frame(
      drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Failures (0)", "Successes (1)")),
      prob = c(mean(param), 1-mean(param))
    )
    ret <- binaryDistrPlotter(analyticalDistr, mean(param), "\\pi", roundDigits = 2)
  }
  ret
}


bernProbitXDraws <- function(params, nObs){
  # takes a 1xn vector of params, returns 1xn draws 
  sapply(params, function(a){bernDraws(a,1)})
}

bernProbitXLikelihoodFun <- function(testParam, outcome, xVals){
  
  nParams <- length(testParam)
  indepVars <- xVals[1:length(outcome),1:nParams]
  
  matrixProduct <- (indepVars %*% testParam)
  paramTransform <- VGAM::probitlink(indepVars %*% testParam, inverse = T)
  nObs <- length(outcome)
  ret <- sum(sapply(1:nObs, function(i){
    outcome[i]*log(paramTransform[i]) + (1 - outcome[i])*log(1-paramTransform[i]) 
  }))
  
  
  ret <- tryCatch({if(ret < -9e20){-9e20} else{ret}}, error = function(e){-9e20})
  
  ret
  
  
}

bernProbitXChartDomain <- function(n){
  d <- lapply(1:n, function(i){list(from = -3, to = 3, by = .01 )})
} 
bernProbitXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Bernoulli",
    pdfTex = " P(y_i|\\pi_i) = \\pi_i^{y_i}(1-\\pi_i)^{(1-y_i)}",
    pdfAddendum = 2,
    modelDistTex = "\\text{Bernoulli}(\\pi_i)",
    modelParamTex = "\\pi_i =  \\Phi(X_i\\beta)  ",
    likelihoodTex = "L(\\beta|y) =  k(y) \\cdot \\prod_{i = 1}^{n} \\left(\\Phi(X_i\\beta) \\right)^{y_i}\\left(1-\\Phi(X_i\\beta)\\right)^{(1-y_i)}",
    logLikelihoodTex = "\\ln[L(\\beta|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln(\\Phi(X_i\\beta)) + }\\) \\({\\small \\sum_{i=1}^{n} (1-y_i) \\ln(1-\\Phi(X_i\\beta))",
    smallLik = 2, 
    smallLL = 2,
    ...
  )
  
}

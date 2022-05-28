#' distrLogic
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
######################################
# Stylized Normal
######################################

styNormPDF <- function(drawVal, param){(2*pi)^(-1/2)* exp(-(1/2)* (drawVal - param)^2)}

styNormParamTransform <- function(p, xVals, DGP = NA){p}

styNormDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- 1} # here to stop an annoying warning
  if(is.null(nObs)){ nObs <- 20} # here to stop an annoying warning

  draws <- rnorm(nObs, param, 1)
}

styNormLikelihoodFun <- function(testParam, outcome, xVals){

  (-1/2)*sum((outcome-testParam)^2)
}


styNormPlotDistr <- function(param, domain, range){
  param <- param[1]
  analyticalDistr <- data.frame(drawVal = seq(domain[1],domain[2],.01)) %>%
    mutate(prob = styNormPDF(drawVal, param))

  continuousDistrPlotter(analyticalDistr, param, '\\beta', roundDigits = 2, arrow = TRUE,
                         xlims = domain, ylims = range, annotationX = param)

}

styNormChartDomain <- function(n){
  d <- lapply(1:n, function(i){list(from = -5, to = 5, by = .01 )})
}


styNormLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Stylized Normal",
    pdfTex = " P(y_i|\\mu_i) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y - \\mu_i)^2}{2} \\right)  ",
    modelDistTex = " f_{stn}(\\mu_i) ",
    modelParamTex = "\\mu_i = \\beta ",
    likelihoodTex = "  L(\\beta|y)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\beta)^2}{2} \\right)",
    logLikelihoodTex = " \\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - \\beta)^2 ",
    smallLik = T,
    ...
  )
}

######################################
# Log Normal
######################################
logNormPDF <- function(drawVal, param){exp(-(1/2)*(log(drawVal) - param)^2 )/(drawVal*sqrt(2*pi))}

logNormParamTransform <- function(p, xVals, DGP = NA){p}

logNormPlotDistr <- function(param, domain, range){
  param <- param[1]
  analyticalDistr <- data.frame(drawVal = seq(domain[1], domain[2],.01)) %>%
    mutate(prob = logNormPDF(drawVal, param))

  continuousDistrPlotter(analyticalDistr, param, '\\mu', roundDigits = 2, arrow = FALSE,
                         xlims = domain, ylims = range)

}

logNormDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- 1} # here to stop an annoying warning
  if(is.null(nObs)){ nObs <- 20} # here to stop an annoying warning
  rlnorm(1:nObs, param)
}

logNormLikelihoodFun <- function(testParam, outcome, xVals){(-1/2)*sum((log(outcome)-testParam)^2)}

logNormChartDomain  <- function(n){
  d <- lapply(1:n, function(i){list(from = -1,to = 3,by = .01)})
}


logNormLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Log  Normal",
    pdfTex = " P(y|\\mu_i) = (y_i\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y_i) - \\mu_i)^2}{2} \\right) ",
    modelDistTex = " \\text{LogNormal}(\\mu_i) ",
    modelParamTex = " \\mu_i = \\beta ",
    likelihoodTex = " L(\\beta|y) = k(y) \\cdot  \\prod_{i = 1}^{n}(y_i\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y_i) - \\beta)^2}{2} \\right)",
    logLikelihoodTex = " \\ln[ L(\\beta|y)] \\, \\dot{=}\\,-\\frac{1}{2} \\sum_{i=1}^{n}  (\\ln (y_i) - \\beta)^2  ",
    smallLik = T,
    ...
  )
}

######################################
# Stylized Normal (X)
######################################

styNormXPDF <- styNormPDF

styNormXParamTransform <- function(p,xVals, DGP = NA){
  if(length(p)!=length(xVals)){ return(1)}
  as.numeric(xVals %*% c(p))}


styNormXPlotDistr <- function(param, domain, range){

  if(is.null(param)){ret <- element_blank()}
  else{
    ret <- multiModelDensity(param = param, domain = domain, pdf = styNormXPDF,
                             paramVal = NA, paramTex = "\\beta", annotationX = NULL, arrow = F, annotate = F,
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


styNormXChartDomain <- styNormChartDomain


styNormXLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Stylized Normal",
    pdfTex = " P(y_i|\\mu_i) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\mu_i)^2}{2} \\right)  ",
    pdfAddendum = 2,
    modelDistTex = " f_{stn}(\\mu_i) ",
    modelParamTex = "\\mu_i = X_i \\beta ",
    likelihoodTex = " L(\\beta|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - X_i\\beta)^2}{2} \\right)",
    logLikelihoodTex = "\\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - X_i\\beta)^2",
    smallLik = T,
    ...
  )
}

######################################
# Log Normal (X)
######################################
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




logNormXDraws <- function(params, nObs, DGP = NA){
  # takes a 1xn vector of params, returns 1xn draws
  sapply(params, function(a){logNormDraws(a,1)})
}

logNormXLikelihoodFun <- function(testParam, outcome, xVals){

  nParams <- length(testParam)
  indepVars <- xVals[1:length(outcome),1:nParams]
  (-1/2)*sum((log(outcome)-(indepVars %*% testParam))^2)

}


logNormXChartDomain <- logNormChartDomain



logNormXLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Log Normal",
    pdfTex = " P(y_i|\\mu_i) = (y_i\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y_i) - \\mu_i)^2}{2} \\right) ",
    pdfAddendum = 2,
    modelDistTex = " \\text{LogNormal}(\\mu_i) ",
    modelParamTex = " \\mu_i = X_i\\beta ",
    likelihoodTex = " L(\\beta|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(\\ln(y_i) - X_i\\beta)^2}{2} \\right)",
    logLikelihoodTex = " \\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (\\ln(y_i) - X_i\\beta)^2",
    smallLik = T,
    ...
  )
}


######################################
# Normal (X)
######################################
######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, sigma has to be last parameter specified here

fullNormXParamTransform <- function(p,xVals, DGP = T){
  pCut <- p[1:(length(p)-1)]
  varianceParam <- p[length(p)]

  if(length(pCut)!=length(xVals)){ return(1)}
  meanParam <- as.numeric(xVals %*% c(pCut))

  if(DGP) {
    return(matrix(c(meanParam, varianceParam), ncol = 2, byrow = F))
  } else {
    return(matrix(c(meanParam, exp(varianceParam)), ncol = 2, byrow = F))
  }
}

fullNormXPDF <- function(drawVal, param){
  (2*pi*param[2]^2)^(-1/2)* exp(-(1/(2*param[2]^2))* (drawVal - param[1])^2)
}

fullNormXPlotDistr <- function(param, domain, range){

  if(is.null(param)){ret <- element_blank()}
  else{
    ret <- multiModelDensity(param = param, domain = domain, pdf = fullNormXPDF,
                             paramVal = NA, paramTex = "\\beta, \\sigma", annotationX = NULL, arrow = F, annotate = F,
                             ylims = range)
  }
  ret
}

fullNormXDraws <- function(params, nObs){
  # pass it a 2 column matrix of params
  if(is.null(params)){params <- matrix(rep(1,40), ncol =2)}
  if(is.null(nObs)){nObs <- 20}
  paramMat <- matrix(params, ncol = 2)
  # takes each row of params, returns a draw from a normal with mu and sigma
  apply(X = paramMat, MARGIN = 1, function(a){rnorm(1,a[1],a[2])})
}

fullNormXLikelihoodFun <- function(testParam, outcome, xVals){

  pCut <- testParam[1:(length(testParam)-1)]
  paramSigma <- exp(testParam[length(testParam)])
  nParams <- length(pCut)
  nObs <- length(outcome)
  indepVars <- xVals[1:nObs,1:nParams]
  paramMu <- as.numeric(indepVars %*% c(pCut))

  -(nObs/2)*(log(paramSigma)) - (1/(2*(paramSigma^2)))*sum((outcome-paramMu)^2)

}


singleChartDomain <- list(from = -5, to = 5, by = .01 )
sigmaChartDomain <- list(from = -2.5, to = 2.5, by = .01 )
fullNormXChartDomain <- function(n){
  d <- lapply(1:n-1, function(i){singleChartDomain})
  d[[n]] <- sigmaChartDomain
  d
}


fullNormXLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Normal",
    pdfTex = "P(y_i|\\mu_i, \\color{blue}{\\sigma}) = (2\\pi\\color{blue}{\\sigma}^2)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\mu_i)^2}{2 \\color{blue}{\\sigma}^2} \\right)  ",
    pdfAddendum = 2,
    modelDistTex = " \\mathcal{N}(\\mu_i, \\sigma^2) ",
    dgpParamTex = "\\mu_i = X_i \\beta ",
    modelParamTex = "\\mu_i = X_i \\beta \\quad \\text{and} \\quad \\sigma = \\exp(\\gamma) ",
    likelihoodTex = " L(\\beta, \\gamma|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi\\exp(\\gamma)^2)^{-1/2} \\text{exp} \\left( \\frac{(y_i - X_i\\beta)^2}{2\\exp(\\gamma)^2} \\right)",
    logLikelihoodTex = "\\ln[ L(\\beta, \\gamma|y, X)] \\, \\dot{=}\\, -n\\gamma -\\frac{1}{2\\exp(\\gamma)^2} \\sum_{i=1}^{n} (y_i - X_i\\beta)^2",
    smallLik = T,
    smallLL = T,
    ...
  )
}


######################################
# Bernoulli
######################################

bernPDF <- function(drawVal, p){p^drawVal * (1-p)^(1-drawVal)}

bernParamTransform <- function(p, xVals, DGP = NA){p}

# domain here is an unused arg for compatibility
bernPlotDistr <- function(param, domain, range){

  param <- param[1]
  if(is.null(param)){ param <- 1} # here to stop an annoying warning
  if(param>1){param <- 1}
  if(param<0){param <- 0}

  analyticalDistr <- data.frame(
    drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Failures (0)", "Successes (1)")),
    prob = c(param, 1-param)
  )

  binaryDistrPlotter(analyticalDistr, param, "\\pi", roundDigits = 2)
}


bernDraws <- function(param, nObs){
  param <- param[1]
  random <- runif(nObs) # n i.i.d. uniform draws
  outcome <- ifelse(random <= param, 1, 0) # how many < pi

  return(outcome)
}

# Function mapping parameters pi to likelihood
bernLikelihoodFun <- function(testParam, outcome, xVals = NULL){

  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  log((testParam^(nSuccesses))*((1-testParam)^(nObs - nSuccesses)))
}

bernChartDomain <- function(n){
  d <- lapply(1:n, function(i){list(from = .01,to = 1,by = .01)})
}

bernLatex <- function(type,...){
  distrLatexFunction(
    type = type,
    modelName = "Bernoulli",
    pdfTex = "P(y_i|\\pi_i) = \\pi_i^{y_i}(1-\\pi_i)^{(1-y_i)}",
    modelDistTex = "\\text{Bernoulli}(\\pi_i)",
    modelParamTex = "\\pi_i = \\pi ",
    likelihoodTex = "L(\\pi|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\pi^{y_i}(1-\\pi)^{(1-y_i)}",
    logLikelihoodTex = "\\ln[L(\\pi|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln(\\pi) + \\sum_{i=1}^{n} (1-y_i) \\ln(1-\\pi)",
    smallLL = T,
    ...
  )

}




######################################
# Bernoulli (Logit)
######################################

bernLogitPDF <- bernPDF

bernLogitParamTransform <- function(p, xVals, DGP = NA){1/(1 + exp(-p))}

bernLogitPlotDistr <- bernPlotDistr

bernLogitDraws <- bernDraws

# Function mapping parameters pi to likelihood
bernLogitLikelihoodFun <- function(testParam, outcome, xVals){

  paramTransform <- 1/(1 + exp(-testParam))

  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  log((paramTransform^(nSuccesses))*((1-paramTransform)^(nObs - nSuccesses)))
}

bernLogitChartDomain <- function(n){
  d <- lapply(1:n, function(i){list(from = -4, to = 4, by = .01 )})
}

bernLogitLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Bernoulli",
    pdfTex = "P(y_i|\\pi_i) = \\pi_i^{y_i}(1-\\pi_i)^{(1-y_i)}",
    pdfAddendum = 1,
    modelDistTex = "\\text{Bernoulli}(\\pi_i)",
    modelParamTex = "\\pi_i = 1/(1 + \\exp(-\\beta)) ",
    likelihoodTex = "L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\left(\\frac{1}{1 + \\exp(-\\beta)}\\right)^{y_i}\\left(1-\\frac{1}{1 + \\exp(-\\beta)}\\right)^{(1-y_i)} ",
    logLikelihoodTex = "\\ln[L(\\beta|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln \\left( \\frac{1}{1 + \\exp(-\\beta)} \\right) + }\\) \\({\\small  \\sum_{i=1}^{n} (1-y_i) \\ln \\left( 1-\\frac{1}{1 + \\exp(-\\beta)} \\right)",
    smallLik = 2,
    smallLL = 2,
    ...
  )

}



######################################
# Bernoulli (Logit, X)
######################################
bernLogitXPDF <- bernPDF

bernLogitXParamTransform <- function(p,xVals, DGP = NA){
  if(length(p)!=length(xVals)){ return(1)}
  1/(1 + exp(- as.numeric(xVals %*% c(p))))
}


bernLogitXPlotDistr <- function(param, domain, range){

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


bernLogitXDraws <- function(params, nObs){
  # takes a 1xn vector of params, returns 1xn draws
  sapply(params, function(a){bernDraws(a,1)})
}

bernLogitXLikelihoodFun <- function(testParam, outcome, xVals){

  nParams <- length(testParam)
  indepVars <- xVals[1:length(outcome),1:nParams]

  matrixProduct <- (indepVars %*% testParam)
  paramTransform <- 1/(1 + exp(-(indepVars %*% testParam)))
  nObs <- length(outcome)
  ret <- sum(sapply(1:nObs, function(i){
    outcome[i]*log(paramTransform[i]) + (1 - outcome[i])*log(1-paramTransform[i])
  }))


  ret <- tryCatch({if(ret < -9e20){-9e20} else{ret}}, error = function(e){0})

  ret


}

bernLogitXChartDomain <- bernLogitChartDomain

bernLogitXLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Bernoulli",
    pdfTex = " P(y_i|\\pi_i) = \\pi_i^{y_i}(1-\\pi_i)^{(1-y_i)}",
    pdfAddendum = 2,
    modelDistTex = "\\text{Bernoulli}(\\pi_i)",
    modelParamTex = "\\pi_i =  1/(1 + \\exp(-X_i\\beta))  ",
    likelihoodTex = "L(\\beta|y) =  k(y) \\cdot \\prod_{i = 1}^{n} \\left(\\frac{1}{1 + \\exp(-X_i\\beta)}\\right)^{y_i}\\left(1-\\frac{1}{1 + \\exp(-X_i\\beta)}\\right)^{(1-y_i)}",
    logLikelihoodTex = "\\ln[L(\\beta|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln \\left( \\frac{1}{1 + \\exp(-X_i\\beta)} \\right)\\, +  }\\) \\({\\small \\sum_{i=1}^{n} (1-y_i) \\ln \\left(1-\\frac{1}{1 + \\exp(-X_i\\beta)} \\right)",
    smallLik = 2,
    smallLL = 2,
    ...
  )

}


######################################
# Bernoulli (Probit, X)
######################################
bernProbitXPDF <- bernPDF

bernProbitXParamTransform <- function(p,xVals, DGP = NA){
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


######################################
# Exponential
######################################


######################################
# Exponential (Exp)
######################################


######################################
# Exponential (Exp, X)
######################################



######################################
# Poisson
######################################



######################################
# Poisson (Exp)
######################################



######################################
# Poisson (Exp, X)
######################################



######################################
# Negative Binomial
######################################


######################################
# Ordered Logit
######################################



######################################
# Ordered Probit
######################################




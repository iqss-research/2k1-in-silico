#' distrLogic
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


# Stylized Normal ---------------------------------------------------------


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


# Log Normal --------------------------------------------------------------


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


# Stylized Normal (X) -----------------------------------------------------


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


# Log Normal X ------------------------------------------------------------

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



# Normal X ----------------------------------------------------------------


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



# Bernoulli ---------------------------------------------------------------


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

##

# Bernoulli (Logit) -------------------------------------------------------


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



# Bernoulli (Logit, X) ----------------------------------------------------


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


# Bernoulli (Probit, X) ---------------------------------------------------


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


# Exponential -------------------------------------------------------------

expPDF <- function(drawVal, param){param*exp(-drawVal*param)}

expParamTransform <- function(p, xVals, DGP = NA){p}

expPlotDistr <- function(param,domain, range){
  param <- param[1]

  analyticalDistr <- data.frame(drawVal = 0:500/100) %>%  mutate(prob = expPDF(drawVal, param))
  continuousDistrPlotter(
    analyticalDistr, param, '\\lambda', roundDigits = 2, arrow = FALSE, ylims = range)

}

expDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- .25} # here to stop an annoying warning
  if(is.null(nObs)){ nObs <- 20} # here to stop an annoying warning
  rexp(1:nObs, param)}

expLikelihoodFun <- function(testParam, outcome, xVals = NULL){sum(log(testParam) - testParam*outcome)}

expChartDomain <- function(n){
  d <- lapply(1:n, function(i){list(from = .01,to = 2.5,by = .01)})
}
expLatex <- function(type,...){
  distrLatexFunction(
    type = type,
    modelName = "Exponential",
    pdfTex = "P(y_i|\\lambda_i) =  \\lambda_i \\exp(-\\lambda_i y_i) ",
    modelDistTex = "\\text{Exponential}(\\lambda_i)",
    modelParamTex = "\\lambda_i = \\lambda ",
    likelihoodTex = "L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n}  \\lambda \\exp(-\\lambda y_i)",
    logLikelihoodTex = "\\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} (\\ln (\\lambda)  - \\lambda y_i)",
    ...
  )

}

# Exponential (Exp) -------------------------------------------------------

expExpPDF <- expPDF

expExpParamTransform <- function(p,xVals, DGP = NA){exp(-p)}

expExpPlotDistr<- expPlotDistr

expExpDraws <- expDraws

expExpLikelihoodFun <- function(testParam, outcome, xVals){
  paramTransform <- exp(-testParam)

  sum(log(paramTransform) - paramTransform*outcome)}

expExpChartDomain  <- function(n){
  d <- lapply(1:n, function(i){list(from = -2, to = 2, by = .01 )})
}


expExpLatex <- function(type,...){
  distrLatexFunction(
    type = type,
    modelName = "Exponential",
    pdfTex = "P(y_i|\\lambda_i) =  \\lambda_i \\exp(-\\lambda_i y_i) ",
    pdfAddendum = 1,
    modelDistTex = "\\text{Exponential}(\\lambda_i)",
    modelParamTex = "\\lambda_i =  \\text{exp}(-\\beta) ",
    likelihoodTex = " L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-\\beta) \\exp(-\\text{exp}(-\\beta) y_i)",
    logLikelihoodTex = "\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (\\beta + \\text{exp}(-\\beta) y_i)",
    ...
  )

}


# Exponential (Exp, X) ----------------------------------------------------
expExpXPDF <- expPDF

expExpXParamTransform <- function(p,xVals, DGP = NA){
  if(length(p)!=length(xVals)){ return(1)}
  paramTransform <- exp(-as.numeric(xVals %*% c(p)))
}


expExpXPlotDistr <- function(param, domain, range){

  if(is.null(param)){ret <- element_blank()}
  else{
    ret <- multiModelDensity(param = param, domain = domain, pdf = expExpXPDF,
                             paramVal = NA, paramTex = "", annotationX = NULL, arrow = F, annotate = F,
                             ylims = range)
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


expExpXChartDomain <- expExpChartDomain

expExpXLatex <- function(type,...){
  distrLatexFunction(
    type = type,
    modelName = "Exponential",
    pdfTex = "P(y_i|\\lambda_i) =  \\lambda_i \\exp(-\\lambda y_i) ",
    pdfAddendum = 2,
    modelDistTex = "\\text{Exponential}(\\lambda_i)",
    modelParamTex = "\\lambda_i =  \\text{exp}(-X_i\\beta) ",
    likelihoodTex = " L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-X_i\\beta) \\exp(-\\text{exp}(-X_i\\beta) y_i) ",
    logLikelihoodTex =
      "\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (X_i\\beta + \\text{exp}(-X_i\\beta) y_i)",
    ...
  )

}



# Poisson -----------------------------------------------------------------

poisPDF <- function(drawVal, param){(param^drawVal)*exp(-param)/(factorial(drawVal))}

poisParamTransform <- function(p, xVals, DGP = NA){p}

poisPlotDistr <- function(param, domain, range){

  param <- param[1]
  analyticalDistr <- data.frame(drawVal = 1:20) %>%  mutate(prob = poisPDF(drawVal, param))

  continuousDistrPlotter(
    analyticalDistr,
    param, '\\lambda',
    roundDigits = 2,
    annotate = TRUE,
    annotationX = param,
    arrow = TRUE, discreteOutput =TRUE,
    ylims = range)

}

poisDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- 1} # here to stop an annoying warning
  if(is.null(nObs)){ nObs <- 20} # here to stop an annoying warning

  if(param<0){param <- 1}
  rpois(1:nObs, param)

}

poisLikelihoodFun <- function(testParam, outcome, xVals){sum(outcome * log(testParam) - testParam)}

poisChartDomain  <- function(n){
  d <- lapply(1:n, function(i){list(from = .01, to = 12, by = .01 )})
}

poisLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Poisson",
    pdfTex = "P(y_i|\\lambda_i) =  \\frac{\\lambda^{y_i}  \\exp(-\\lambda)}{y_i!}  \\; \\text{if} \\; y_i \\in \\mathbb{N}, \\; 0 \\; \\text{otherwise}   ",
    modelDistTex = "\\text{Poisson}(\\lambda_i)",
    modelParamTex = "\\lambda_i = \\lambda ",
    likelihoodTex = " L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\lambda^{y_i}  \\exp(-\\lambda)}{y_i!}",
    logLikelihoodTex = " \\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\ln(\\lambda)  - \\lambda \\right) ",
    ...
  )

}


# Poisson (Exp) -----------------------------------------------------------


poisExpPDF <- poisPDF

poisExpParamTransform <- function(p,xVals, DGP = NA){exp(p)}

poisExpPlotDistr <- function(param, domain, range){

  param <- param[1]
  analyticalDistr <- data.frame(drawVal = 1:20)
  analyticalDistr <- analyticalDistr %>%  mutate(prob = poisExpPDF(drawVal, param))

  continuousDistrPlotter(
    analyticalDistr,
    param, '\\beta', roundDigits = 2, arrow = FALSE, discreteOutput =TRUE, ylims = range)

}

poisExpDraws <- poisDraws

poisExpLikelihoodFun <- function(testParam, outcome, xVals){

  paramTransform <- exp(testParam)
  sum(outcome * log(paramTransform) - paramTransform)
}

poisExpChartDomain  <- function(n){
  d <- lapply(1:n, function(i){list(from = -4, to = 4, by = .01 )})
}

poisExpLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Poisson",
    pdfTex = "P(y_i|\\lambda_i) =  \\frac{\\lambda_i^{y_i}  \\exp(-\\lambda_i)}{y_i!} \\; \\text{if} \\; y_i \\in \\mathbb{N}, \\; 0 \\; \\text{otherwise}   ",
    pdfAddendum = 1,
    modelDistTex = "\\text{Poisson}(\\lambda_i)",
    modelParamTex = "\\lambda_i = \\text{exp}(\\beta)  ",
    likelihoodTex = "L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\text{exp}(\\beta)^{y_i}  \\text{exp}(-\\text{exp}(\\beta))}{y_i!}",
    logLikelihoodTex = " \\ln[ L(\\beta|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\beta  - \\text{exp}(\\beta) \\right) ",
    smallLik = T,
    ...
  )

}

# Poisson (Exp, X) --------------------------------------------------------

poisExpXPDF <- poisPDF

poisExpXParamTransform <- function(p,xVals, DGP = NA){
  if(length(p)!=length(xVals)){ return(1)}
  exp(as.numeric(xVals %*% c(p)))
}

poisExpXPlotDistr <- function(param, domain, range){

  if(is.null(param)){ret <- element_blank()}
  else{
    ret <- multiModelDensity(param = param, domain = domain, pdf = poisExpXPDF,
                             paramVal = NA, paramTex = "\\beta", annotationX = NULL, arrow = F, annotate = F,
                             ylims = range)
  }
  ret
}


poisExpXDraws <- function(params, nObs){
  # takes a 1xn vector of params, returns 1xn draws
  sapply(params, function(a){poisDraws(a,1)})
}

poisExpXLikelihoodFun <- function(testParam, outcome, xVals){

  nParams <- length(testParam)
  indepVars <- xVals[1:length(outcome),1:nParams]

  paramTransform <- exp(indepVars %*% testParam)
  sum(outcome * log(paramTransform) - paramTransform)

}


poisExpXChartDomain <- poisExpChartDomain


poisExpXLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Poisson",
    pdfTex = "P(y_i|\\lambda_i) =  \\frac{\\lambda_i^{y_i} \\exp(-\\lambda_i)}{y_i!} \\; \\text{if} \\; y_i \\in \\mathbb{N}, \\; 0 \\; \\text{otherwise}  ",
    pdfAddendum = 2,
    modelDistTex = "\\text{Poisson}(\\lambda_i)",
    modelParamTex = "\\lambda_i =  \\text{exp}(X_i \\beta)  ",
    likelihoodTex = "L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\text{exp}(X_i\\beta)^{y_i}  \\text{exp}(-\\text{exp}(X_i\\beta))}{y_i!}",
    logLikelihoodTex = " \\ln[ L(\\beta|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  X_i\\beta  - \\text{exp}(X_i\\beta) \\right)",
    smallLik = T,
    ...
  )

}



# Negative Binomial -------------------------------------------------------

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
  d <- lapply(1:n-1, function(i){singleChartDomain})
  d[[n]] <- sigmaChartDomain
  d
}


negBinomXLatex <- function(type, ...){
  distrLatexFunction(
    type = type,
    modelName = "Negative Binomial",
    pdfTex = "{\\small P(y_i|\\lambda_i, \\color{blue}{\\sigma}^2) = \\frac{\\Gamma (\\frac{\\lambda_i}{\\color{blue}{\\sigma}^2 -1} +y_i )}{y_i! \\Gamma (\\frac{\\lambda_i}{\\color{blue}{\\sigma}^2 -1 })}\\left(\\frac{\\color{blue}{\\sigma}^2 - 1}{\\color{blue}{\\sigma}^2}\\right)^{y_i}(\\color{blue}{\\sigma}^2)^{\\frac{-\\lambda_i}{\\color{blue}{\\sigma}^2 - 1}} }",
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


# Ordered Probit ----------------------------------------------------------
######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, exactly two betas and one gamma (plus implicit tau = 0)
# nBetas <- 2
machineConst <-  .Machine$double.eps*10

orderedProbitXParamTransform <- function(p,xVals, DGP = T){

  betaVals <- p[1:(length(p)-1)]
  gammaVals <- p[length(p)]
  if(length(betaVals)!=length(xVals)){ return(1)}
  muParam <- as.numeric(xVals %*% c(betaVals))
  tauParams <- if(DGP) {
    Reduce(x = gammaVals, f = function(i,j){
      c(i, tail(i,1) + machineConst + j)}, init = 0)
  } else {
    Reduce(x = gammaVals, f = function(i,j){
      c(i, tail(i,1) + machineConst + exp(j))}, init = 0) }

  return(matrix(c(muParam, tauParams), ncol = length(tauParams)+1, byrow = F))

}

orderedProbitXPDF <- function(drawVal, param){
  muParam <- param[1]
  tauParams <- param[2:length(param)]
  relativeParams <- tauParams - muParam

  lowerBound <- if(drawVal >1) {relativeParams[drawVal-1]} else{-9999}
  upperBound <- if(drawVal <= length(relativeParams)) {relativeParams[drawVal]} else{9999}

  probitlink(upperBound, inverse = T) - probitlink(lowerBound, inverse = T)
}

orderedProbitXPlotDistr <- function(param, domain, range){

  if(is.null(param)){return( element_blank())}
  else{

    domain <- seq(domain[1], domain[2])
    probs <- sapply(domain, function(a){
      mean(sapply(1:nrow(param), function(b){orderedProbitXPDF(a, param[b,])}))
    })
    distrDF <- data.frame(drawVal = domain, prob = probs)

    paramTex <- "\\beta, \\gamma"

    ret <- ggplot(distrDF, aes(x = drawVal, y = prob, fill = drawVal)) +
      geom_bar(stat="identity", alpha = .5) +
      scale_fill_gradientn(colors = cbPalette[1:3]) +
      labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
      theme_minimal() +
      ylim(0,1.1) +
      theme(text = element_text(family = "sans"),
            legend.position = "none",
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
            axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5)
      )

    suppressWarnings({ggplot_build(ret)})
  }

}

orderedProbitXDraws <- function(params, nObs){

  if(is.null(params)){params <- matrix(rep(1,40), ncol =2)}
  if(is.null(nObs)){nObs <- 20}
  paramMat <- matrix(params, ncol = 3)
  muParam <- paramMat[,1]
  tauParams <- paramMat[,2:ncol(paramMat)] - muParam
  probs <- probitlink(cbind(-9999,
                            matrix(tauParams, ncol = 2),9999), inverse = T)

  sapply(1:nrow(paramMat), function(i){
    sample(1:3, prob = diff(probs[i,]), size = 1, replace = TRUE)
  })

}

orderedProbitXLikelihoodFun <- function(testParam, outcome, xVals){
  transformedTest <- sapply(1:nrow(xVals), function(i){
    orderedProbitXParamTransform(p = testParam, xVals = xVals[i,], DGP = F)}) %>%  t()
  vec <- sapply(1:length(outcome), function(i){
    orderedProbitXPDF(drawVal = outcome[i], param = transformedTest[i,])
  } )

  sum(log(vec))

}


singleChartDomain <- list(from = -5, to = 5, by = .01 )
orderedProbitXChartDomain <- function(n){
  d <- lapply(1:n, function(i){singleChartDomain})
}


orderedProbitXLatex <- function( type,
                                 nXValsPDF = 1,
                                 nXValsAssumed = 1,
                                 xValsSim = c(),
                                 paramValsPDF = c(),
                                 nParamLL = 0,
                                 paramTex = "",
                                 metaParamTex = "",
                                 smallLik = 0,
                                 smallLL = 0,
                                 nObs = 0, ...){
  if(type == "Distr") {

    xStrs <- paste(lapply(1:nXValsPDF, function(i){
      tmpXStr <- if(nXValsPDF > 1){paste0("X_{i,",i,"}")} else {"X_i"}
      paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")

    div(
      tags$p(tags$b("Probability Model"), style = "padding-bottom:15px"),
      tags$p(withMathJax(paste0("\\(\\hspace{30px} Y^\\text{*}_i \\sim \\mathcal{N}(\\mu_i, 1) \\quad \\text{where} \\, i = 1, \\ldots, n \\)"))),
      tags$p(paste0(
        "\\( \\hspace{30px} \\mu_i = X_i \\beta, \\; \\text{and} \\; X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)")),
      tags$p("\\( \\hspace{30px} Y^\\text{*}_i \\perp \\!\\!\\! \\perp Y^\\text{*}_j \\quad \\forall \\: i \\neq j \\)"),
      tags$p(paste0("\\( \\hspace{30px}  y_i= \\begin{cases}
    1 &\\text{if}& y^\\text{*}_i < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq y^\\text{*}_i < \\tau_1 \\\\
    3 &\\text{if}& \\tau_1 \\leq y^\\text{*}_i  \\\\
    \\end{cases} \\)")),
      tags$p(paste0("\\( \\hspace{30px} 0 = \\tau_0 < \\tau_1 \\)")),
    )


  } else if (type == "Model") {

    xStrs <- paste(lapply(1:nXValsAssumed, function(i){
      tmpXStr <- if(nXValsAssumed > 1){paste0("X_{i,",i,"}")} else {"X_i"}
      paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")

    div(tags$p(tags$b("Statistical Model ")),
        tags$p(withMathJax(paste0("\\( \\hspace{30px} Y^\\text{*}_i \\sim \\mathcal{N}(\\mu_i, 1)  \\)"))),
        tags$p(paste0(
          "\\( \\hspace{30px}\\mu_i =  X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)")),
        tags$p("\\( \\hspace{30px} Y^\\text{*}_i \\perp \\!\\!\\! \\perp Y^\\text{*}_j \\quad \\forall \\: i \\neq j \\)"),
        tags$p(paste0("\\( \\hspace{30px}  y_i= \\begin{cases}
    1 &\\text{if}& y^\\text{*}_i < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq y^\\text{*}_i < \\tau_1 \\\\
    3 &\\text{if}& \\tau_1 \\leq y^\\text{*}_i  \\\\
    \\end{cases} \\)")),
    )

  } else if (type == "Likelihood"){

    div(tags$p(tags$b(withMathJax("Likelihood for data \\(\\small y = (y_1, \\dots,y_n)\\) :"))),
        tags$p(paste0(" \\(\\hspace{30px} {\\small L(\\beta, \\gamma|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} [\\text{Pr}(Y_i = j)]} \\)")),
        tags$p(tags$small("\\( \\hspace{30px} \\) where \\( k(y) \\) is an unknown function of the data: see", tags$a(href = "https://projects.iq.harvard.edu/2k1-in-silico/notation", target = "_blank", "docs"))),
        tags$p(tags$b("Log Likelihood:")),
        tags$p(paste0("\\(\\hspace{30px} {\\small \\ln[ L(\\beta, \\gamma|y, X)] \\, \\dot{=}\\, \\ln[F_{stn}(\\exp(\\gamma_j)|x_i\\beta) -  F_{stn}(\\exp(\\gamma_{j-1})|x_i\\beta)] } \\)")))


  } else if(type == "Estimation Uncertainty"){

    div(
      tags$p(tags$b("Estimation Uncertainty")),
      tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{\\beta} \\sim \\mathcal{N}(\\hat{\\beta}, \\hat{V}(\\hat{\\beta})) \\)")))
    )

  } else if(type == "Fundamental Uncertainty"){

    if(!is.numeric(xValsSim[[1]])) {return(div())} else{
      tmpXStr <- if(nXValsAssumed > 1){paste0("X_{c,",i,"}")} else {"X_c"}
      xStrs <- paste(lapply(1:length(xValsSim), function(i){
        paste0(" + \\tilde{\\beta_",i,"}",tmpXStr)}), collapse = "")}


    prefaceStr <- " \\tilde{\\mu_c} = X_c \\tilde{\\beta} = \\tilde{\\beta_0} "


    ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
               tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{Y}^\\text{*}_c \\sim \\mathcal{N}(\\tilde{\\mu_c}, 1)  \\)"))),
               tags$p(paste0("\\(  \\hspace{30px} \\",prefaceStr,xStrs, "\\)")),
               tags$p(paste0("\\( \\hspace{30px}  \\tilde{y}_c= \\begin{cases}
    1 &\\text{if}& \\tilde{y}^\\text{*}_c < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq \\tilde{y}^\\text{*}_c < \\tilde{\\tau_1} \\\\
    3 &\\text{if}& \\tilde{\\tau_1} \\leq \\tilde{y}^\\text{*}_c  \\\\
    \\end{cases} \\)")),
    )}
}



# Ordered Logit -----------------------------------------------------------

######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, exactly two betas and one gamma (plus implicit tau = 0)
machineConst <-  .Machine$double.eps*10

styLogPDF <- function(drawVal, param){exp(drawVal - param)/((1+exp(drawVal - param))^2)}

orderedLogitXParamTransform <- function(p,xVals, DGP = T){

  betaVals <- p[1:(length(p)-1)]
  gammaVals <- p[length(p)]

  if(length(betaVals)!=length(xVals)){ return(1)}
  muParam <- as.numeric(xVals %*% c(betaVals))

  tauParams <- if(DGP) {
    Reduce(x = gammaVals, f = function(i,j){
      c(i, tail(i,1) + machineConst + j)}, init = 0)
  } else {
    Reduce(x = gammaVals, f = function(i,j){
      c(i, tail(i,1) + machineConst + exp(j))}, init = 0) }

  return(matrix(c(muParam, tauParams), ncol = length(tauParams)+1, byrow = F))

}

orderedLogitXPDF <- function(drawVal, param){
  muParam <- param[1]
  tauParams <- param[2:length(param)]
  relativeParams <- tauParams - muParam

  lowerBound <- if(drawVal >1) {relativeParams[drawVal-1]} else{-9999}
  upperBound <- if(drawVal <= length(relativeParams)) {relativeParams[drawVal]} else{9999}

  logitlink(upperBound, inverse = T) - logitlink(lowerBound, inverse = T)
}

orderedLogitXPlotDistr <- function(param, domain, range){

  if(is.null(param)){return( element_blank())}
  else{

    domain <- seq(domain[1], domain[2])
    probs <- sapply(domain, function(a){
      mean(sapply(1:nrow(param), function(b){orderedLogitXPDF(a, param[b,])}))
    })
    distrDF <- data.frame(drawVal = domain, prob = probs)

    paramTex <- "\\beta, \\gamma"

    ret <- ggplot(distrDF, aes(x = drawVal, y = prob, fill = drawVal)) +
      geom_bar(stat="identity", alpha = .5) +
      scale_fill_gradientn(colors = cbPalette[1:3]) +
      labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
      theme_minimal() +
      ylim(0,1.1) +
      theme(text = element_text(family = "sans"),
            legend.position = "none",
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
            axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5)
      )

    suppressWarnings({ggplot_build(ret)})
  }

}

orderedLogitXDraws <- function(params, nObs){

  if(is.null(params)){params <- matrix(rep(1,40), ncol =2)}
  if(is.null(nObs)){nObs <- 20}
  paramMat <- matrix(params, ncol = 3)
  muParam <- paramMat[,1]
  tauParams <- paramMat[,2:ncol(paramMat)] - muParam
  probs <- logitlink(cbind(-9999,
                           matrix(tauParams, ncol = 2),9999), inverse = T)
  if(any(isnothing(probs)))(return(c()))
  sapply(1:nrow(paramMat), function(i){
    sample(1:3, prob = diff(probs[i,]), size = 1, replace = TRUE)
  })

}

orderedLogitXLikelihoodFun <- function(testParam, outcome, xVals){
  transformedTest <- sapply(1:nrow(xVals), function(i){
    orderedLogitXParamTransform(p = testParam, xVals = xVals[i,], DGP = F)}) %>%  t()
  vec <- sapply(1:length(outcome), function(i){
    orderedLogitXPDF(drawVal = outcome[i], param = transformedTest[i,])
  } )

  sum(log(vec))

}


singleChartDomain <- list(from = -5, to = 5, by = .01 )
orderedLogitXChartDomain <- function(n){
  d <- lapply(1:n, function(i){singleChartDomain})
}


orderedLogitXLatex <- function( type,
                                nXValsPDF = 1,
                                nXValsAssumed = 1,
                                xValsSim = c(),
                                paramValsPDF = c(),
                                nParamLL = 0,
                                paramTex = "",
                                metaParamTex = "",
                                smallLik = 0,
                                smallLL = 0,
                                nObs = 0, ...){
  if(type == "Distr") {

    xStrs <- paste(lapply(1:nXValsPDF, function(i){
      tmpXStr <- if(nXValsPDF > 1){paste0("X_{i,",i,"}")} else {"X_i"}
      paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")

    div(
      tags$p(tags$b("Probability Model"), style = "padding-bottom:15px"),
      tags$p(withMathJax(paste0("\\(\\hspace{30px} Y^\\text{*}_i \\sim \\text{STL}(\\mu_i) \\quad \\text{where} \\, i = 1, \\ldots, n \\)"))),
      tags$p(paste0(
        "\\( \\hspace{30px} \\mu_i = X_i \\beta, \\; \\text{and} \\; X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)")),
      tags$p("\\( \\hspace{30px} Y^\\text{*}_i \\perp \\!\\!\\! \\perp Y^\\text{*}_j \\quad \\forall \\: i \\neq j \\)"),
      tags$p(paste0("\\( \\hspace{30px}  y_i= \\begin{cases}
    1 &\\text{if}& y^\\text{*}_i < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq y^\\text{*}_i < \\tau_1 \\\\
    3 &\\text{if}& \\tau_1 \\leq y^\\text{*}_i  \\\\
    \\end{cases} \\)")),
      tags$p(paste0("\\( \\hspace{30px} 0 = \\tau_0 < \\tau_1 \\)"))
    )


  } else if (type == "Model") {

    xStrs <- paste(lapply(1:nXValsAssumed, function(i){
      tmpXStr <- if(nXValsAssumed > 1){paste0("X_{i,",i,"}")} else {"X_i"}
      paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")

    div(tags$p(tags$b("Statistical Model ")),
        tags$p(withMathJax(paste0("\\( \\hspace{30px} Y^\\text{*}_i \\sim \\text{STL}(\\mu_i)  \\)"))),
        tags$p(paste0(
          "\\( \\hspace{30px} \\mu_i = X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)")),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"),
        tags$p(paste0("\\( \\hspace{30px}  y_i= \\begin{cases}
    1 &\\text{if}& y^\\text{*}_i < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq y^\\text{*}_i < \\tau_1 \\\\
    3 &\\text{if}& \\tau_1 \\leq y^\\text{*}_i  \\\\
    \\end{cases} \\)")),
    )

  } else if (type == "Likelihood"){

    div(tags$p(tags$b(withMathJax("Likelihood for data \\(\\small y = (y_1, \\dots,y_n)\\) :"))),
        tags$p(paste0(" \\(\\hspace{30px} {\\small L(\\beta, \\gamma|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} [\\text{Pr}(Y_i = j)]} \\)")),
        tags$p(tags$small("\\( \\hspace{30px} \\) where \\( k(y) \\) is an unknown function of the data: see", tags$a(href = "https://projects.iq.harvard.edu/2k1-in-silico/notation", target = "_blank", "docs"))),
        tags$p(tags$b("Log Likelihood:")),
        tags$p(paste0("\\(\\hspace{30px} {\\small \\ln[ L(\\beta, \\gamma|y, X)] \\, \\dot{=}\\, \\ln[F_{stl}(\\exp(\\gamma_j)|x_i\\beta) -  F_{stl}(\\exp(\\gamma_{j-1})|x_i\\beta)] } \\)")))


  } else if(type == "Estimation Uncertainty"){

    div(
      tags$p(tags$b("Estimation Uncertainty")),
      tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{\\beta} \\sim \\mathcal{N}(\\hat{\\beta}, \\hat{V}(\\hat{\\beta})) \\)")))
    )

  } else if(type == "Fundamental Uncertainty"){

    if(!is.numeric(xValsSim[[1]])) {return(div())} else{
      tmpXStr <- if(nXValsAssumed > 1){paste0("X_{c,",i,"}")} else {"X_c"}
      xStrs <- paste(lapply(1:length(xValsSim), function(i){
        paste0(" + \\tilde{\\beta_",i,"}",tmpXStr)}), collapse = "")}


    prefaceStr <- " \\tilde{\\mu}_c = X_c \\tilde{\\beta} = \\tilde{\\beta_0} "


    ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
               tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{Y}^\\text{*}_c \\sim \\text{STL}(\\tilde{\\mu_c})  \\)"))),
               tags$p(paste0("\\(  \\hspace{30px} \\",prefaceStr,xStrs, "\\)")),
               tags$p(paste0("\\( \\hspace{30px}  \\tilde{y}_c= \\begin{cases}
    1 &\\text{if}& \\tilde{y}^\\text{*}_c < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq \\tilde{y}^\\text{*}_c < \\tilde{\\tau_1} \\\\
    3 &\\text{if}& \\tilde{\\tau_1}\\leq \\tilde{y}^\\text{*}_c  \\\\
    \\end{cases} \\)")),
    )}
}


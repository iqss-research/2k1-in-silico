bernParamTransform <- function(p, xVals){p}

# domain here is an unused arg for compatibility
bernPlotDistr <- function(param, domain, range){
  
  param <- param[1]
  
  if(param>1){param <- 1}
  if(param<0){param <- 0}
  
  analyticalDistr <- data.frame(
    drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Successes (1)", "Failures (0)")),
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

singleChartDomain <- list(from = .01,to = 1,by = .01)
bernChartDomain <- list(singleChartDomain)


bernLatex <- function(type,...){
  distrLatexFunction(
    type = type, 
    modelName = "Bernoulli",
    pdfTex = "P(y|\\pi) = \\pi^y(1-\\pi)^{(1-y)}",
    modelDistTex = "\\text{Bernoulli}(\\pi_i)",
    modelParamTex = "\\pi_i = \\pi ",
    likelihoodTex = "L(\\pi|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\pi^{y_i}(1-\\pi)^{(1-y_i)}",
    logLikelihoodTex = "\\ln[L(\\pi|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln(\\pi) + \\sum_{i=1}^{n} (1-y_i) \\ln(1-\\pi)",
    smallLL = T,
    ...
  )
  
}



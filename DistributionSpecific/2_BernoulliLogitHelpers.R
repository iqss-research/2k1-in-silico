bernLogitPDF <- bernPDF

bernLogitParamTransform <- function(p, xVals){1/(1 + exp(-p))}

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
    logLikelihoodTex = "\\ln[L(\\beta|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln(\\frac{1}{1 + \\exp(-\\beta)}) + }\\) \\({\\small  \\sum_{i=1}^{n} (1-y_i) \\ln(1-\\frac{1}{1 + \\exp(-\\beta)})",
    smallLik = 2,
    smallLL = 2,
    ...
  )
  
}


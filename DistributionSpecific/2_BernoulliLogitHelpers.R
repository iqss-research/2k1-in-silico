bernLogitSlider <- manyParamSliderMaker(
  minVal = -2, maxVal = 2, startVals = c(1.3), stepVal = .1, paramHTML = "&beta;", multi = F)



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

singleChartDomain <- seq(-4,4,.01)
bernLogitChartDomain <- expand.grid(singleChartDomain)



bernLogitLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Bernoulli",
    pdfTex = "P(y|\\pi) = \\pi^y(1-\\pi)^{(1-y)}",
    pdfAddendum = 1,
    modelDistTex = "\\text{Bernoulli}(\\pi_i)",
    modelParamTex = "\\pi_i = 1/(1 + \\exp(-\\beta)) ",
    likelihoodTex = "L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\left( \\frac{1}{1 + \\exp(-\\beta)} \\right)^{y_i} \\cdot \\left(  \\frac{\\exp(-\\beta)}{1 + \\exp(-\\beta)} \\right)^{(1-y_i)} ",
    logLikelihoodTex = "\\ln[L(\\beta|y)] \\dot{=}   -\\sum_{i=1}^{n} \\ln(1+ \\text{exp}(-\\beta[1-2y_i]))",
    smallLik = 2,
    ...
  )
  
}


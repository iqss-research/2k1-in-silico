poisExpPDF <- poisPDF

poisExpParamTransform <- function(p,xVals){exp(p)}

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
    pdfTex = "P(y|\\lambda) =  \\frac{\\lambda^y  \\exp(-\\lambda)}{y!}  ",
    pdfAddendum = 1,
    modelDistTex = "\\text{Poisson}(\\lambda_i)",
    modelParamTex = "\\lambda_i = \\text{exp}(\\beta)  ",
    likelihoodTex = "L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\text{exp}(\\beta)^{y_i}  \\text{exp}(-\\text{exp}(\\beta))}{y_i!}",
    logLikelihoodTex = " \\ln[ L(\\beta|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\beta  - \\text{exp}(\\beta) \\right) ",
    smallLik = T,
    ...
  )
  
}

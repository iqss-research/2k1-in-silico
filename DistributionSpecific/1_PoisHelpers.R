poisSlider <- manyParamSliderMaker(
  minVal = 1, maxVal = 10, startVals = c(2), stepVal = 1, paramHTML = "&lambda;", multi = F)


poisParamTransform <- function(p, xVals){p}


poisPlotDistr <- function(param){
  
  param <- param[1]
  analyticalDistr <- data.frame(drawVal = 1:20)
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (param^drawVal)*exp(-param)/(factorial(drawVal)))
  
  continuousDistrPlotter(analyticalDistr, param, '\\beta', roundDigits = 2, arrow = FALSE, discreteOutput =TRUE)

}

poisDraws <- function(param, nObs){
  param <- param[1]
  
  if(param<0){param <- 1}
  rpois(1:nObs, param)
  
  }

poisLikelihoodFun <- function(testParam, outcome, xVals){sum(outcome * log(testParam) - testParam)}

singleChartDomain <- seq(.1,12,.1)
poisChartDomain <- expand.grid(singleChartDomain)


poisLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Poisson",
    pdfTex = "P(y|\\lambda) =  \\frac{\\lambda^y  \\exp(-\\lambda)}{y!}  ",
    modelDistTex = "\\text{Poisson}(\\lambda_i)",
    modelParamTex = "\\lambda_i = \\lambda ",
    likelihoodTex = " L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\lambda^{y_i}  \\exp(-\\lambda)}{y_i!}",
    logLikelihoodTex = " \\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\ln(\\lambda)  - \\lambda \\right) ",
    ...
  )
  
}

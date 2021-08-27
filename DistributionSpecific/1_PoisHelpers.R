

poisSlider <- sliderInput("param1",
                          div(HTML("Choose &lambda;:")),
                          min = 1,
                          max = 10,
                          value = 2,
                          step = 1,
                          width = paramSliderWidth)


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

poisLikelihoodFun <- function(testParam, outcome){sum(outcome * log(testParam) - testParam)}

singleChartDomain <- seq(.1,12,.1)
poisChartDomain <- expand.grid(singleChartDomain)


poisLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Exponential",
    pdfTex = "P(y|\\lambda) =  \\frac{\\lambda^y  \\exp(-\\lambda)}{y!}  ",
    modelDistTex = "\\text{Poisson}(\\lambda_i)",
    modelParamTex = "\\lambda_i = \\lambda ",
    likelihoodTex = " L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\lambda^{y_i}  \\exp(-\\lambda)}{y_i!}",
    logLikelihoodTex = " \\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\ln(\\lambda)  - \\lambda \\right) ",
    ...
  )
  
}

expSlider <- sliderInput("param1",
                         div(HTML("Choose &lambda;:")),
                         min = 0,
                         max = 2,
                         value = .25,
                         step = .25,
                         width = paramSliderWidth)

expParamTransform <- function(p, xVals){p}


expPlotDistr <- function(param){
  param <- param[1]
  
  analyticalDistr <- data.frame(drawVal = 0:500/100) %>%  mutate(prob = param*exp(-drawVal*param))
  continuousDistrPlotter(analyticalDistr, param, '\\lambda', roundDigits = 2, arrow = FALSE)
  
}

expDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- .25} # here to stop an annoying warning
  rexp(1:nObs, param)}

expLikelihoodFun <- function(testParam, outcome){sum(log(testParam) - testParam*outcome)}

singleChartDomain <- seq(.01,2.5,.01)
expChartDomain <- expand.grid(singleChartDomain)

expLatex <- function(type,...){
  distrLatexFunction(
    type = type, 
    modelName = "Exponential",
    pdfTex = "P(y|\\lambda) =  \\lambda \\exp(-\\lambda y) ",
    modelDistTex = "\\text{Exponential}(\\lambda_i)",
    modelParamTex = "\\lambda_i = \\lambda ",
    likelihoodTex = "L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n}  \\lambda \\exp(-\\lambda y_i)",
    logLikelihoodTex = "\\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} (\\ln (\\lambda)  - \\lambda y_i)",
    ...
  )
  
}



poisExpSlider <- sliderInput("param1",
                             div(HTML("Choose &beta;:")),
                             min = -.25,
                             max = 3,
                             value = 1,
                             step = .25,
                             width = paramSliderWidth)

poisExpParamTransform <- function(p,xVals){exp(p)}

poisExpPlotDistr <- poisPlotDistr

poisExpDraws <- poisDraws

poisExpLikelihoodFun <- function(testParam, outcome){
  
  paramTransform <- exp(testParam)
  sum(outcome * log(paramTransform) - paramTransform)
}

singleChartDomain <- seq(-4,4,.01)
poisExpChartDomain <- expand.grid(singleChartDomain)


poisExpLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Exponential",
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

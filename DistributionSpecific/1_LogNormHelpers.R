logNormSlider <- sliderInput("param1",
                             div(HTML("Choose &beta;:")),
                             min = -1,
                             max = 2,
                             value = 1,
                             step = .25,
                             width = paramSliderWidth)

logNormParamTransform <- function(p, xVals){p}

logNormPlotDistr <- function(param){
  param <- param[1]
  analyticalDistr <- data.frame(drawVal = 1:5000/500)
  analyticalDistr <- analyticalDistr %>%  mutate(prob = exp(-(1/2)*(log(drawVal) - param)^2 )/(drawVal*sqrt(2*pi)))
  
  continuousDistrPlotter(analyticalDistr, param, '\\beta', roundDigits = 2, arrow = FALSE)
  
}

logNormDraws <- function(param, nObs){
  param <- param[1]
  rlnorm(1:nObs, param)
}

logNormLikelihoodFun <- function(testParam, outcome){(-1/2)*sum((log(outcome)-testParam)^2)}

singleChartDomain <- seq(-2,2,.01)
logNormChartDomain <- expand.grid(singleChartDomain)


logNormLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Log  Normal",
    pdfTex = " P(y|\\beta) = (y\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y) - \\beta)^2}{2} \\right) ",
    modelDistTex = " \\text{LogNormal}(\\mu_i) ",
    modelParamTex = " \\mu_i = \\beta ",
    likelihoodTex = " L(\\beta|y) = k(y) \\cdot  \\prod_{i = 1}^{n}(y_i\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y_i) - \\beta)^2}{2} \\right)",
    logLikelihoodTex = " \\ln[ L(\\beta|y)] \\, \\dot{=}\\,-\\frac{1}{2} \\sum_{i=1}^{n}  (\\ln (y_i) - \\beta)^2  ",
    smallLik = T,
    ...
  )
}
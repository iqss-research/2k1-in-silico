styNormSlider <- manyParamSliderMaker(
  minVal = -2, maxVal = 2, startVals = c(1), stepVal = .25, paramHTML = "&beta;", multi = F)

styNormParamTransform <- function(p, xVals){p}

styNormPlotDistr <- function(param){
  param <- param[1]
  analyticalDistr <- data.frame(drawVal = -300:300/100 + param)
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (drawVal - param)^2))
  
  continuousDistrPlotter(analyticalDistr, param, '\\beta', roundDigits = 2, arrow = TRUE)

  
}

styNormDraws <- function(param, nObs){
  
  param <- param[1]
  draws <- rnorm(nObs, param, 1)
  
}

styNormLikelihoodFun <- function(testParam, outcome, xVals){
  
  (-1/2)*sum((outcome-testParam)^2)
}

singleChartDomain <- seq(from = -5, to = 5, by = .01 )
styNormChartDomain <- expand.grid(singleChartDomain)


styNormLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Stylized Normal",
    pdfTex = " P(y|\\beta) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y - \\beta)^2}{2} \\right)  ",
    modelDistTex = " f_{stn}(\\mu_i) ",
    modelParamTex = "\\mu_i = \\beta ",
    likelihoodTex = "  L(\\beta|y)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\beta)^2}{2} \\right)",
    logLikelihoodTex = " \\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - \\beta)^2 ",
    smallLik = T,
    ...
  )
}
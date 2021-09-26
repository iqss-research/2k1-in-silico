logNormPDF <- function(drawVal, param){exp(-(1/2)*(log(drawVal) - param)^2 )/(drawVal*sqrt(2*pi))}

logNormParamTransform <- function(p, xVals){p}

logNormPlotDistr <- function(param, domain, range){
  param <- param[1]
  analyticalDistr <- data.frame(drawVal = seq(domain[1], domain[2],.01)) %>%  
    mutate(prob = logNormPDF(drawVal, param))
  
  continuousDistrPlotter(analyticalDistr, param, '\\mu', roundDigits = 2, arrow = FALSE,
                         xlims = domain, ylims = range)
  
}

logNormDraws <- function(param, nObs){
  param <- param[1]
  rlnorm(1:nObs, param)
}

logNormLikelihoodFun <- function(testParam, outcome, xVals){(-1/2)*sum((log(outcome)-testParam)^2)}

singleChartDomain <- list(from = -1,to = 3,by = .01)
logNormChartDomain <- list(singleChartDomain)


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
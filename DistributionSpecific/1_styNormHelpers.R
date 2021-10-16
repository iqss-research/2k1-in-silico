styNormPDF <- function(drawVal, param){(2*pi)^(-1/2)* exp(-(1/2)* (drawVal - param)^2)}

styNormParamTransform <- function(p, xVals){p}

styNormDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- 1} # here to stop an annoying warning
  if(is.null(nObs)){ nObs <- 20} # here to stop an annoying warning
  
  draws <- rnorm(nObs, param, 1)
}

styNormLikelihoodFun <- function(testParam, outcome, xVals){
  
  (-1/2)*sum((outcome-testParam)^2)
}


styNormPlotDistr <- function(param, domain, range){
  param <- param[1]
  analyticalDistr <- data.frame(drawVal = seq(domain[1],domain[2],.01)) %>% 
    mutate(prob = styNormPDF(drawVal, param))
  
  continuousDistrPlotter(analyticalDistr, param, '\\beta', roundDigits = 2, arrow = TRUE, 
                         xlims = domain, ylims = range, annotationX = param)
  
}

styNormChartDomain <- function(n){
  d <- lapply(1:n, function(i){list(from = -5, to = 5, by = .01 )})
} 


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
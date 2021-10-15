poisPDF <- function(drawVal, param){(param^drawVal)*exp(-param)/(factorial(drawVal))}

poisParamTransform <- function(p, xVals){p}

poisPlotDistr <- function(param, domain, range){
  
  param <- param[1]
  analyticalDistr <- data.frame(drawVal = 1:20) %>%  mutate(prob = poisPDF(drawVal, param))
  
  continuousDistrPlotter(
    analyticalDistr,
    param, '\\lambda',
    roundDigits = 2,
    annotate = TRUE,
    annotationX = param,
    arrow = TRUE, discreteOutput =TRUE,
    ylims = range)

}

poisDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- 1} # here to stop an annoying warning
  if(is.null(nObs)){ nObs <- 20} # here to stop an annoying warning
  
  if(param<0){param <- 1}
  rpois(1:nObs, param)
  
  }

poisLikelihoodFun <- function(testParam, outcome, xVals){sum(outcome * log(testParam) - testParam)}

singleChartDomain <- list(from = .01,to = 12,by = .01)
poisChartDomain <- list(singleChartDomain)


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

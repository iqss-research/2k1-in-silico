expPDF <- function(drawVal, param){param*exp(-drawVal*param)}

expParamTransform <- function(p, xVals){p}

expPlotDistr <- function(param,domain, range){
  param <- param[1]
  
  analyticalDistr <- data.frame(drawVal = 0:500/100) %>%  mutate(prob = expPDF(drawVal, param))
  continuousDistrPlotter(
    analyticalDistr, param, '\\lambda', roundDigits = 2, arrow = FALSE, ylims = range)
  
}

expDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- .25} # here to stop an annoying warning
  if(is.null(nObs)){ nObs <- 20} # here to stop an annoying warning
  rexp(1:nObs, param)}

expLikelihoodFun <- function(testParam, outcome, xVals = NULL){sum(log(testParam) - testParam*outcome)}

singleChartDomain <- list(from = .01,to = 2.5,by = .01)
expChartDomain <- list(singleChartDomain)

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

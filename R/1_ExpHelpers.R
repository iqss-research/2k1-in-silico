expPDF <- function(drawVal, param){param*exp(-drawVal*param)}

expParamTransform <- function(p, xVals, DGP = NA){p}

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

expChartDomain <- function(n){
  d <- lapply(1:n, function(i){list(from = .01,to = 2.5,by = .01)})
} 
expLatex <- function(type,...){
  distrLatexFunction(
    type = type, 
    modelName = "Exponential",
    pdfTex = "P(y_i|\\lambda_i) =  \\lambda_i \\exp(-\\lambda_i y_i) ",
    modelDistTex = "\\text{Exponential}(\\lambda_i)",
    modelParamTex = "\\lambda_i = \\lambda ",
    likelihoodTex = "L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n}  \\lambda \\exp(-\\lambda y_i)",
    logLikelihoodTex = "\\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} (\\ln (\\lambda)  - \\lambda y_i)",
    ...
  )
  
}

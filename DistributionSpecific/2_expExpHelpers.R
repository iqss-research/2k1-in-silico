expExpPDF <- expPDF

expExpParamTransform <- function(p,xVals){exp(-p)}

expExpPlotDistr<- expPlotDistr

expExpDraws <- expDraws

expExpLikelihoodFun <- function(testParam, outcome, xVals){
  paramTransform <- exp(-testParam)
  
  sum(log(paramTransform) - paramTransform*outcome)}

expExpChartDomain  <- function(n){
  d <- lapply(1:n, function(i){list(from = -2, to = 2, by = .01 )})
} 


expExpLatex <- function(type,...){
  distrLatexFunction(
    type = type, 
    modelName = "Exponential",
    pdfTex = "P(y_i|\\lambda_i) =  \\lambda_i \\exp(-\\lambda_i y_i) ",
    pdfAddendum = 1,
    modelDistTex = "\\text{Exponential}(\\lambda_i)",
    modelParamTex = "\\lambda_i =  \\text{exp}(-\\beta) ",
    likelihoodTex = " L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-\\beta) \\exp(-\\text{exp}(-\\beta) y_i)",
    logLikelihoodTex = "\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (\\beta + \\text{exp}(-\\beta) y_i)",
    ...
  )
  
}

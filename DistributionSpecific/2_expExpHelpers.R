expExpParamTransform <- function(p,xVals){exp(-p)}

expExpPlotDistr<- expPlotDistr

expExpDraws <- expDraws

expExpLikelihoodFun <- function(testParam, outcome, xVals){
  paramTransform <- exp(-testParam)
  
  sum(log(paramTransform) - paramTransform*outcome)}

singleChartDomain <- seq(-2,2,.01)
expExpChartDomain <- expand.grid(singleChartDomain)


expExpLatex <- function(type,...){
  distrLatexFunction(
    type = type, 
    modelName = "Exponential",
    pdfTex = "P(y|\\lambda) =  \\lambda \\exp(-\\lambda y) ",
    pdfAddendum = 1,
    modelDistTex = "\\text{Exponential}(\\lambda_i)",
    modelParamTex = "\\lambda_i =  \\text{exp}(-\\beta) ",
    likelihoodTex = " L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-\\beta) \\exp(-\\text{exp}(-\\beta) y_i)",
    logLikelihoodTex = "\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (\\beta + \\text{exp}(-\\beta) y_i)",
    ...
  )
  
}

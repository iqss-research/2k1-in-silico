logNormXSlider <- column(12,
                          sliderInput("param1",
                                      label = div(HTML("Choose &beta;<sub>0</sub>:")),
                                      min = -1,
                                      max = 2,
                                      value = 1,
                                      step = .25,
                                      width = paramSliderWidth),
                          sliderInput("param2",
                                      div(HTML("&beta;<sub>1</sub>:")),
                                      min = -1,
                                      max = 2,
                                      value = -1,
                                      step = .25,
                                      width = paramSliderWidth),
                          sliderInput("param3",
                                      div(HTML("&beta;<sub>2</sub>:")),
                                      min = -1,
                                      max = 2,
                                      value = 0,
                                      step = .25,
                                      width = paramSliderWidth),
                          tags$p("Choose Observation"),
                          fluidRow(column(width = 5, selectInput(inputId = "xRow",
                                                                 label = NULL,
                                                                 choices = 1:200,
                                                                 selected = 1,
                                                                 width = "100px")),
                                   column(width = 7, tags$div(id = 'placeholder')))
)


logNormXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  as.numeric(xVals %*% c(p))
}



logNormXPlotDistr <- function(param){
  
  if(is.null(param)){ret <- element_blank()}
  else{
    
    analyticalDistr <- data.frame(drawVal = seq(0.01,10,.01))
    analyticalDistr <- analyticalDistr %>%  mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (log(drawVal) - param)^2))
    
    ret <- continuousDistrPlotter(analyticalDistr, param, "\\beta", annotationX = param)
    
  }
  
  ret
}




logNormXDraws <- logNormDraws

logNormXLikelihoodFun <- function(testParam, outcome){
  
  nParams <- length(testParam)
  indepVars <- indepVarsBase[1:length(outcome),1:nParams]
  (-1/2)*sum((log(outcome)-(indepVars %*% testParam))^2)
  
}


singleChartDomain <- seq(from = -5, to = 5, by = .05 )
logNormXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)




logNormXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Log  Normal",
    pdfTex = " P(y|\\beta) = (y\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y) - \\beta)^2}{2} \\right) ",
    pdfAddendum = 2,
    modelDistTex = " \\text{LogNormal}(\\mu_i) ",
    modelParamTex = " \\mu_i = X_i\\beta ",
    likelihoodTex = " L(\\beta|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(\\ln(y_i) - X_i\\beta)^2}{2} \\right)",
    logLikelihoodTex = " \\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (\\ln(y_i) - X_i\\beta)^2",
    smallLik = T,
    ...
  )
}
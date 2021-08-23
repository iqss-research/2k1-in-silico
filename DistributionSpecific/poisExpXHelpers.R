poisExpXSlider <- column(12,
                          sliderInput("param1",
                                      label = div(HTML("Choose &beta;<sub>0</sub>:")),
                                      min = -.5,
                                      max = .5,
                                      value = .3,
                                      step = .1),
                          sliderInput("param2",
                                      div(HTML("&beta;<sub>1</sub>:")),
                                      min = -.5,
                                      max = .5,
                                      value = 0,
                                      step = .1),
                          sliderInput("param3",
                                      div(HTML("&beta;<sub>2</sub>:")),
                                      min = -.5,
                                      max = .5,
                                      value = .3,
                                      step = .1),
                          tags$p("Choose Observation"),
                          fluidRow(column(width = 5, selectInput(inputId = "xRow",
                                                                 label = NULL,
                                                                 choices = 1:200,
                                                                 selected = 1,
                                                                 width = "100px")),
                                   column(width = 7, tags$div(id = 'placeholder')))
)

poisExpXParamTransform <- function(p,xVals){
  exp(as.numeric(xVals %*% c(p)))
}


poisExpXPlotDistr <- function(param){
  
  if(is.null(param)){ret <- element_blank()}
  else{

    analyticalDistr <- data.frame(drawVal = seq(0,30,2))
    analyticalDistr <- analyticalDistr %>%  mutate(prob = (param^drawVal)*exp(-param)/(factorial(drawVal)))
    
    ret <- continuousDistrPlotter(analyticalDistr, param, '\\lambda', roundDigits = 2, arrow = FALSE, discreteOutput = TRUE)
  }
  
  ret
}


poisExpXDraws <- poisDraws

poisExpXLikelihoodFun <- function(testParam, outcome){
  
  nParams <- length(testParam)
  indepVars <- indepVarsBase[1:length(outcome),1:nParams]
  
  paramTransform <- exp(indepVars %*% testParam)
  sum(outcome * log(paramTransform) - paramTransform)
  
}


singleChartDomain <- seq(from = -.5, to = .5, by = .01 )
poisExpXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)




poisExpXLatex <- function(type){
  
  if(type == "Distr"){
    
    div(
      withMathJax("$${\\large P(y_i |\\beta) =  \\frac{\\lambda_i^{y_i}  \\exp(-\\lambda_i)}{y_i !} }$$
                $$\\text{where} \\quad \\lambda_i = \\exp(X_i \\beta) = \\beta_0 + \\beta_1 X_{i,1} + \\beta_2 X_{i,2} $$"),
      tags$small("with X fixed: see", tags$a("Notation", onclick="customHref('Notation')"))
    )
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Stylized Normal \\begin{aligned}
Y_i &\\sim \\text{Poisson}(\\lambda_i) \\\\
\\lambda_i &= \\text{exp}(X_i \\beta)   \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\; \\;|X \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
     Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$  L(\\beta|y) = k(y) \\cdot $$
                $$\\prod_{i = 1}^{n} \\frac{\\text{exp}(X_i\\beta)^{y_i}  \\text{exp}(-\\text{exp}(X_i\\beta))}{y_i!}  $$
                Log Likelihood: $${\\ln[ L(\\beta|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  X_i\\beta  - \\text{exp}(X_i\\beta) \\right)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
logNormXSlider <- column(12,
                          sliderInput("param1",
                                      label = div(HTML("Choose &beta;<sub>0</sub>:")),
                                      min = -1,
                                      max = 2,
                                      value = 1,
                                      step = .25),
                          sliderInput("param2",
                                      div(HTML("&beta;<sub>1</sub>:")),
                                      min = -1,
                                      max = 2,
                                      value = -1,
                                      step = .25),
                          sliderInput("param3",
                                      div(HTML("&beta;<sub>2</sub>:")),
                                      min = -1,
                                      max = 2,
                                      value = 0,
                                      step = .25),
                          tags$p("Choose Observation"),
                          fluidRow(column(width = 5, selectInput(inputId = "xRow",
                                                                 label = NULL,
                                                                 choices = 1:200,
                                                                 selected = 1,
                                                                 width = "100px")),
                                   column(width = 7, tags$div(id = 'placeholder')))
)


logNormXParamTransform <- function(p,xVals){
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




logNormXLatex <- function(type){
  
  if(type == "Distr"){
    
    div(
      withMathJax("$${\\large P(y_i|\\beta) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(\\ln(y_i) - \\mu_i)^2}{2} \\right) }$$
                $$\\text{where} \\quad \\mu_i = X_i \\beta = \\beta_0 + \\beta_1 X_{i,1} + \\beta_2 X_{i,2} $$"),
      tags$small("with X fixed: see", tags$a("Notation", onclick="customHref('Notation')"))
    )
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Stylized Normal \\begin{aligned}
Y_i &\\sim \\text{LogNormal} (y_i |\\mu_i)\\\\
\\mu_i &= X_i \\beta   \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\; \\;|X \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$  L(\\beta|y, X)= k(y) \\cdot $$ $$\\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(\\ln(y_i) - X_i\\beta)^2}{2} \\right) $$
                Log Likelihood: $${\\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (\\ln(y_i) - X_i\\beta)^2 }$$")
    
  } else stop("Unknown Markdown!")
  
  
}
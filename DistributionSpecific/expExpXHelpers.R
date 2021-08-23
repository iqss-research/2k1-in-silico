expExpXSlider <- column(12,
                        sliderInput("param1",
                                    label = div(HTML("Choose &beta;<sub>0</sub>:")),
                                    min = -.5,
                                    max = .5,
                                    value = .2,
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
                                    value = -.2,
                                    step = .1),
                        tags$p("Choose Observation"),
                        fluidRow(column(width = 5, selectInput(inputId = "xRow",
                                                               label = NULL,
                                                               choices = 1:200,
                                                               selected = 1,
                                                               width = "100px")),
                                 column(width = 7, tags$div(id = 'placeholder')))
)



expExpXPlotDistr <- function(param, xRow){
  
  if(is.null(param) || is.null(xRow)){ret <- element_blank()}
  else{
    nParams <- length(param)
    xVals <- indepVarsBase[xRow, 1:nParams]
    
    paramTransform <- exp(-as.numeric(xVals %*% c(param)))

    analyticalDistr <- data.frame(drawVal = 0:500/100)
    analyticalDistr <- analyticalDistr %>%  mutate(prob = paramTransform*exp(-drawVal*paramTransform))
    
    ret <- continuousDistrPlotter(analyticalDistr, paramTransform, '\\lambda',roundDigits = 2, arrow = FALSE)   
  }
  
  ret
}

expExpXDraws <- function(param, nObs, xRow = 1, xVals = NULL){
  
  nParams <- length(param)
  if(!is.null(xRow)){
    indepVars <- indepVarsBase[xRow:nObs,1:nParams]
  } else {  indepVars <- xVals}
  
  paramTransform <- exp(-indepVars %*% param)
  outcome <- rexp(1:nObs, paramTransform)
  
  return(outcome)
}

expExpXLikelihoodFun <- function(testParam, outcome){
  
  nParams <- length(testParam)
  indepVars <- indepVarsBase[1:length(outcome),1:nParams]
  
  paramTransform <- exp(-indepVars %*% testParam)
  sum(log(paramTransform) - paramTransform*outcome)
}


singleChartDomain <- seq(from = -2, to = 2, by = .01 )
expExpXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)




expExpXLatex <- function(type){
  
  if(type == "Distr"){
    
    div(
      withMathJax("$${\\large P(y_i|\\beta) =  \\lambda_i \\exp(-\\lambda_i y) }$$
                $$\\text{where} \\quad \\lambda_i = \\text{exp}(X_i \\beta) = \\beta_0 + \\beta_1 X_{i,1} + \\beta_2 X_{i,2} $$"),
      tags$small("with X fixed: see", tags$a("Notation", onclick="customHref('Notation')"))
    )
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Stylized Normal \\begin{aligned}
Y_i &\\sim \\text{Exponential}(\\lambda_i) \\\\
\\lambda_i &= \\text{exp}(-X_i \\beta)   \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\; \\;|X \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
               Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-X_i\\beta) \\exp(-\\text{exp}(-X_i\\beta) y_i)  $$
                Log Likelihood: $${\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (X_i\\beta + \\text{exp}(-X_i\\beta) y_i)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
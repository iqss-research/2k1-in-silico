bernLogitXSlider <- column(12,
                          sliderInput("param1",
                                      label = div(HTML("Choose &beta;<sub>0</sub>:")),
                                      min = -2,
                                      max = 2,
                                      value = 1,
                                      step = .25),
                          sliderInput("param2",
                                      div(HTML("Choose &beta;<sub>1</sub>:")),
                                      min = -2,
                                      max = 2,
                                      value = -1,
                                      step = .25),
                          sliderInput("param3",
                                      div(HTML("Choose &beta;<sub>2</sub>:")),
                                      min = -2,
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

bernLogitXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  1/(1 + exp(- as.numeric(xVals %*% c(p))))
}



bernLogitXPlotDistr <- function(param){
  
  if(is.null(param)){ret <- element_blank()}
  else{

    analyticalDistr <- data.frame(
      drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Successes (1)", "Failures (0)")),
      prob = c(param, 1-param)
    )
    
    ret <- binaryDistrPlotter(analyticalDistr, param, "\\pi", roundDigits = 2)
    
  }
  
  ret
}




bernLogitXDraws <- bernDraws

bernLogitXLikelihoodFun <- function(testParam, outcome){
  
  nParams <- length(testParam)
  indepVars <- indepVarsBase[1:length(outcome),1:nParams]
  
  paramTransform <- 1/(1 + exp(-(indepVars %*% testParam)))
  
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  ret <- sum(log((paramTransform^(nSuccesses))*((1-paramTransform)^(nObs - nSuccesses))))
  
  if(ret < -9e20){ret <- -9e20}
  
  ret
  
  
}


singleChartDomain <- seq(from = -2, to = 2, by = .05 )
bernLogitXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)




bernLogitXLatex <- function(type){
  
  if(type == "Distr"){
    
    div(
      withMathJax("$${\\large  P(y_i|\\beta) = \\pi_i^{y_i}(1-\\pi_i)^{{(1-y_i)}} }$$
      $${\\text{where} \\quad \\pi_i = \\frac{{1}}{{1 + \\text{exp}(-X_i\\beta)}} \\quad \\text{and} \\quad  X_i\\beta =  \\beta_0 + \\beta_1 X_{i,1} + \\beta_2 X_{i,2} }$$"),
      tags$small("with X fixed: see", tags$a("Notation", onclick="customHref('Notation')"))
    )
  } else if(type == "Model"){
    div(tags$p(withMathJax("Statistical Model: Bernoulli")),
        tags$p("\\( \\hspace{30px} Y_i \\sim \\text{Bernoulli}(\\pi_i) \\)"),
        tags$p("\\( \\hspace{30px} \\pi_i = 1/(1 + \\text{exp}(-X_i\\beta)) \\)"),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))
    
  } else if(type == "Likelihood"){
    div(tags$p(withMathJax("Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :")),
        tags$p(" \\(\\hspace{30px}  L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\left ( \\frac{{1}}{{1 + \\text{exp}(-X_i\\beta)}}\\right)^{y_i} \\)"),
        tags$p(" \\(\\hspace{30px} \\cdot \\left(  \\frac{{\\text{exp}(-X_i\\beta)}}{{1 + \\text{exp}(-X_i\\beta)}} \\right )^{{(1-y_i)}} \\)"),
        tags$p("Log Likelihood:"),
        tags$p("\\(\\hspace{30px} \\ln[L(\\beta|y)] \\dot{=}   -\\sum_{i=1}^{n} \\ln(1+ \\text{exp}(-X_i\\beta[1-2y_i]))  \\)"))
    
  } else stop("Unknown Markdown!")
  
}
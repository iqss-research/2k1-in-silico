

bernLogitSlider <- sliderInput("param1",
                          div(HTML("Choose &beta;:")),
                          min = -3,
                          max = 3,
                          value = 1.2,
                          step = .1)

bernLogitParamTransform <- function(p, xVals){1/(1 + exp(-p))}


bernLogitPlotDistr <- bernPlotDistr


bernLogitDraws <- bernDraws

# Function mapping parameters pi to likelihood
bernLogitLikelihoodFun <- function(testParam, outcome){
  
  paramTransform <- 1/(1 + exp(-testParam))
  
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  log((paramTransform^(nSuccesses))*((1-paramTransform)^(nObs - nSuccesses)))
}

singleChartDomain <- seq(-5,5,.01)
bernLogitChartDomain <- expand.grid(singleChartDomain)

bernLogitLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large  P(y|\\pi) = \\pi^y(1-\\pi)^{{(1-y)}} \\quad \\text{where} \\quad \\pi =  \\frac{{1}}{{1 + \\text{exp}(-\\beta)}} }$$")
    
  }
  else if(type == "Model"){
    div(tags$p(withMathJax("Statistical Model: Bernoulli")),
        tags$p("\\( \\hspace{30px} Y_i \\sim \\text{Bernoulli}(\\pi_i) \\)"),
        tags$p("\\( \\hspace{30px} \\pi_i = 1/(1 + \\text{exp}(-\\beta))  \\)"),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))
    
  } else if(type == "Likelihood"){
    div(tags$p(withMathJax("Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :")),
        tags$p(" \\(\\hspace{30px} {\\small L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\left( \\frac{1}{1 + \\exp(-\\beta)} \\right)^{y_i} \\cdot \\left(  \\frac{\\exp(-\\beta)}{1 + \\exp(-\\beta)} \\right)^{(1-y_i)} }\\)"),
        tags$p("Log Likelihood:"),
        tags$p("\\(\\hspace{30px} \\ln[L(\\beta|y)] \\dot{=}   -\\sum_{i=1}^{n} \\ln(1+ \\text{exp}(-\\beta[1-2y_i]))  \\)"))
    
  } else stop("Unknown Markdown!")
  
}
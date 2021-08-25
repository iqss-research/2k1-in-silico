

poisExpSlider <- sliderInput("param1",
                             div(HTML("Choose &beta;:")),
                             min = -.25,
                             max = 3,
                             value = 1,
                             step = .25)

poisExpParamTransform <- function(p,xVals){exp(p)}

poisExpPlotDistr <- poisPlotDistr

poisExpDraws <- poisDraws

poisExpLikelihoodFun <- function(testParam, outcome){
  
  paramTransform <- exp(testParam)
  sum(outcome * log(paramTransform) - paramTransform)
}

singleChartDomain <- seq(-4,4,.01)
poisExpChartDomain <- expand.grid(singleChartDomain)

poisExpLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\frac{\\lambda^y  \\exp(-\\lambda)}{y!}  \\quad \\text{where} \\quad \\lambda = \\text{exp}(\\beta)}$$")
    
  } else if(type == "Model"){
    
    div(tags$p(withMathJax("Statistical Model: Poisson")),
        tags$p("\\( \\hspace{30px} Y_i \\sim \\text{Poisson}(\\lambda_i) \\)"),
        tags$p("\\( \\hspace{30px} \\lambda_i = \\text{exp}(\\beta)  \\)"),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))
    
  } else if(type == "Likelihood"){
    
    div(tags$p(withMathJax("Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :")),
        tags$p(" \\(\\hspace{30px} {\\small   L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\text{exp}(\\beta)^{y_i}  \\text{exp}(-\\text{exp}(\\beta))}{y_i!} } \\)"),
        tags$p("Log Likelihood:"),
        tags$p("\\(\\hspace{30px} \\ln[ L(\\beta|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\beta  - \\text{exp}(\\beta) \\right) \\)"))
  } else stop("Unknown Markdown!")  
  
}
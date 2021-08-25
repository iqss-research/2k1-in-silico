expSlider <- sliderInput("param1",
                         div(HTML("Choose &lambda;:")),
                         min = 0,
                         max = 2,
                         value = .25,
                         step = .25)

expParamTransform <- function(p, xVals){p}


expPlotDistr <- function(param){
  param <- param[1]
  
  analyticalDistr <- data.frame(drawVal = 0:500/100) %>%  mutate(prob = param*exp(-drawVal*param))
  continuousDistrPlotter(analyticalDistr, param, '\\lambda', roundDigits = 2, arrow = FALSE)
  
}

expDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- .25} # here to stop an annoying warning
  rexp(1:nObs, param)}

expLikelihoodFun <- function(testParam, outcome){sum(log(testParam) - testParam*outcome)}

singleChartDomain <- seq(.01,2.5,.01)
expChartDomain <- expand.grid(singleChartDomain)

expLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\lambda \\exp(-\\lambda y)  }$$")
    
  } else if(type == "Model"){
    
    div(tags$p(withMathJax("Statistical Model: Exponential")),
        tags$p("\\( \\hspace{30px} Y_i \\sim \\text{Exponential}(\\lambda_i) \\)"),
        tags$p("\\( \\hspace{30px} \\lambda_i = \\lambda  \\)"),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))
    
  } else if(type == "Likelihood"){
    
    div(tags$p(withMathJax("Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :")),
        tags$p(" \\(\\hspace{30px} L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n}  \\lambda \\exp(-\\lambda y_i) \\)"),
        tags$p("Log Likelihood:"),
        tags$p("\\(\\hspace{30px} \\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} (\\ln (\\lambda)  - \\lambda y_i) \\)"))
  } else stop("Unknown Markdown!")
  
  
}
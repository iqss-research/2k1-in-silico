expExpSlider <- sliderInput("param1",
                            div(HTML("Choose &beta;:")),
                            min = -2,
                            max = 2,
                            value = .25,
                            step = .25)


expExpParamTransform <- function(p,xVals){exp(-p)}

expExpPlotDistr<- expPlotDistr

expExpDraws <- expDraws

expExpLikelihoodFun <- function(testParam, outcome){
  paramTransform <- exp(-testParam)
  
  sum(log(paramTransform) - paramTransform*outcome)}

singleChartDomain <- seq(-2,2,.01)
expExpChartDomain <- expand.grid(singleChartDomain)


expExpLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\lambda \\exp(-\\lambda y)  \\quad \\text{where} \\quad \\lambda = \\text{exp}(-\\beta) }$$")
    
  } else if(type == "Model"){
    
    div(tags$p(withMathJax("Statistical Model: Exponential")),
        tags$p("\\( \\hspace{30px} Y_i \\sim \\text{Exponential}(\\lambda_i) \\)"),
        tags$p("\\( \\hspace{30px} \\lambda_i =  \\text{exp}(-\\beta)   \\)"),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))
    
  } else if(type == "Likelihood"){
    
    div(tags$p(withMathJax("Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :")),
        tags$p(" \\(\\hspace{30px} L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-\\beta) \\exp(-\\text{exp}(-\\beta) y_i) \\)"),
        tags$p("Log Likelihood:"),
        tags$p("\\(\\hspace{30px} \\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (\\beta + \\text{exp}(-\\beta) y_i) \\)"))
  } else stop("Unknown Markdown!")

  
}
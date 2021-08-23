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
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Exponential \\begin{aligned}
Y_i &\\sim \\text{Exponential}(\\lambda_i) \\\\
\\lambda_i &= \\text{exp}(-\\beta) \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-\\beta) \\exp(-\\text{exp}(-\\beta) y_i)  $$
                Log Likelihood: $${\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (\\beta + \\text{exp}(-\\beta) y_i)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
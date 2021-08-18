expExpSlider <- sliderInput("param1",
                            div(HTML("Choose &beta;:")),
                            min = -2,
                            max = 2,
                            value = .25,
                            step = .25)

expExpPlotDistr<- function(param, xRow=1){
  param <- param[1]
  paramTransform <- exp(-param)
  
  analyticalDistr <- data.frame(drawVal = 0:500/100)
  analyticalDistr <- analyticalDistr %>%  mutate(prob = paramTransform*exp(-drawVal*paramTransform))
  
  continuousDistrPlotter(analyticalDistr, paramTransform, '\\lambda',roundDigits = 2, arrow = FALSE)
  
  
}

expExpDraws <- function(param, nObs, xRow = 1){
  param <- param[1]
  if(is.null(param)){ param <- .25} # here to stop an annoying warning
  paramTransform <- exp(-param)
  rexp(1:nObs, paramTransform)}

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
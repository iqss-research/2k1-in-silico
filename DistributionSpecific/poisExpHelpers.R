

poisExpSlider <- sliderInput("param1",
                             div(HTML("Choose &beta;:")),
                             min = -.25,
                             max = 3,
                             value = 1,
                             step = .25)

poisExpParamTransform <- function(p,xRow){exp(p)}

poisExpPlotDistr <- function(param, xRow=1){
  
  param <- param[1]
  paramTransform <- exp(param)
  
  analyticalDistr <- data.frame(drawVal = 0:30)
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (paramTransform^drawVal)*exp(-paramTransform)/(factorial(drawVal)))

  continuousDistrPlotter(analyticalDistr, paramTransform, '\\lambda', roundDigits = 2, arrow = FALSE, discreteOutput = TRUE)
  
}

poisExpDraws <- function(param, nObs, xRow = 1, xVals = NULL){
  
  param <- param[1]
  paramTransform <- exp(param)
  rpois(1:nObs, paramTransform)
  
}

poisExpLikelihoodFun <- function(testParam, outcome){
  
  paramTransform <- exp(testParam)
  sum(outcome * log(paramTransform) - paramTransform)
}

singleChartDomain <- seq(-4,4,.01)
poisExpChartDomain <- expand.grid(singleChartDomain)

poisExpLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\frac{\\lambda^y  \\exp(-\\lambda)}{y!}  \\quad \\text{where} \\quad \\lambda = \\text{exp}(\\beta)}$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Poisson \\begin{aligned}
Y_i &\\sim \\text{Poisson}(\\lambda_i) \\\\
\\lambda_i &= \\text{exp}(\\beta)  \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$  L(\\beta|y) = k(y) \\cdot $$
                $$\\prod_{i = 1}^{n} \\frac{\\text{exp}(\\beta)^{y_i}  \\text{exp}(-\\text{exp}(\\beta))}{y_i!}  $$
                Log Likelihood: $${\\ln[ L(\\beta|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\beta  - \\text{exp}(\\beta) \\right)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
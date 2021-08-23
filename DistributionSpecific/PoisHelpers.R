

poisSlider <- sliderInput("param1",
                          div(HTML("Choose &lambda;:")),
                          min = 1,
                          max = 10,
                          value = 2,
                          step = 1)


poisPlotDistr <- function(param, xRow=1){
  
  param <- param[1]
  
  analyticalDistr <- data.frame(drawVal = 1:20)
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (param^drawVal)*exp(-param)/(factorial(drawVal)))
  
  continuousDistrPlotter(analyticalDistr, param, '\\beta', roundDigits = 2, arrow = FALSE, discreteOutput =TRUE)

}

poisDraws <- function(param, nObs, xRow = 1, xVals = NULL){
  param <- param[1]
  
  if(param<0){param <- 1}
  rpois(1:nObs, param)
  
  }

poisLikelihoodFun <- function(testParam, outcome){sum(outcome * log(testParam) - testParam)}

singleChartDomain <- seq(.1,12,.1)
poisChartDomain <- expand.grid(singleChartDomain)

poisLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\frac{\\lambda^y  \\exp(-\\lambda)}{y!}  }$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Poisson \\begin{aligned}
Y_i &\\sim \\text{Poisson}(\\lambda_i) \\\\
\\lambda_i &= \\lambda  \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\lambda^{y_i}  \\exp(-\\lambda)}{y_i!}  $$
                Log Likelihood: $${\\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\ln(\\lambda)  - \\lambda \\right)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
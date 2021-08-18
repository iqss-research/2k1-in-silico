styNormSlider <- sliderInput("param1",
                             div(HTML("Choose &beta;:")),
                             min = -2,
                             max = 2,
                             value = 1,
                             step = .25)

styNormPlotDistr <- function(param, xRow=1){
  param <- param[1]
  
  analyticalDistr <- data.frame(
    drawVal = -300:300/100 + param
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (drawVal - param)^2))
  
  
  continuousDistrPlotter(analyticalDistr, param, '\\beta', roundDigits = 2, arrow = TRUE)

  
}




styNormDraws <- function(param, nObs, xRow = 1){
  
  param <- param[1]
  draws <- rnorm(nObs, param, 1)
  
}

styNormLikelihoodFun <- function(testParam, outcome){
  
  (-1/2)*sum((outcome-testParam)^2)
}

singleChartDomain <- seq(from = -5, to = 5, by = .01 )
styNormChartDomain <- expand.grid(singleChartDomain)



styNormLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\beta) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y - \\beta)^2}{2} \\right) }$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Stylized Normal \\begin{aligned}
Y_i &\\sim f_{\\text{stn}}(y_i |\\mu_i) \\\\
\\mu_i &= \\beta  \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$  L(\\beta|y)= k(y) \\cdot $$ $$\\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\beta)^2}{2} \\right) $$
                Log Likelihood: $${\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - \\beta)^2 }$$")
    
  } else stop("Unknown Markdown!")
  
  
}
logNormSlider <- sliderInput("param1",
                             div(HTML("Choose &beta;:")),
                             min = -1,
                             max = 2,
                             value = 1,
                             step = .25)

logNormPlotDistr <- function(param, xRow=1){
  param <- param[1]
  
  
  analyticalDistr <- data.frame(
    drawVal = 1:5000/500
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = exp(-(1/2)*(log(drawVal) - param)^2 )/(drawVal*sqrt(2*pi)))
  

  continuousDistrPlotter(analyticalDistr, param, '\\beta', roundDigits = 2, arrow = FALSE)
  
}

logNormDraws <- function(param, nObs){
  param <- param[1]
  rlnorm(1:nObs, param)
}

logNormLikelihoodFun <- function(testParam, outcome){(-1/2)*sum((log(outcome)-testParam)^2)}

singleChartDomain <- seq(-2,2,.01)
logNormChartDomain <- expand.grid(singleChartDomain)

logNormLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\beta) = (y\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y) - \\beta)^2}{2} \\right) }$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Log Normal \\begin{aligned}
Y_i &\\sim \\text{LogNormal} (y_i |\\mu_i) \\\\
\\mu_i &= \\beta  \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$  L(\\beta|y) = k(y) \\cdot $$ $$\\prod_{i = 1}^{n}(y_i\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y_i) - \\beta)^2}{2} \\right) $$
                Log Likelihood: $${\\ln[ L(\\beta|y)] \\, \\dot{=}\\,-\\frac{1}{2} \\sum_{i=1}^{n}  (\\ln (y_i) - \\beta)^2  }$$")
    
  } else stop("Unknown Markdown!")
  
}
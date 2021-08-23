

bernSlider <- sliderInput("param1",
              div(HTML("Choose &pi;:")),
              min = 0,
              max = 1,
              value = .3,
              step = .1)

bernParamTransform <- function(p, xRow){p}


bernPlotDistr <- function(param, xRow){
  
  param <- param[1]
  
  if(param>1){param <- 1}
  
  analyticalDistr <- data.frame(
    drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Successes (1)", "Failures (0)")),
    prob = c(param, 1-param)
  )
  
  binaryDistrPlotter(analyticalDistr, param, "\\pi", roundDigits = 2)
}


bernDraws <- function(param, nObs, xRow = 1, xVal = NULL){
  param <- param[1]
  random <- runif(nObs) # n i.i.d. uniform draws
  outcome <- ifelse(random <= param, 1, 0) # how many < pi
  
  return(outcome)
}

# Function mapping parameters pi to likelihood
bernLikelihoodFun <- function(testParam, outcome){
  
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  log((testParam^(nSuccesses))*((1-testParam)^(nObs - nSuccesses)))
}

singleChartDomain <- seq(.01,1,.01)
bernChartDomain <- expand.grid(singleChartDomain)


bernLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\pi) = \\pi^y(1-\\pi)^{{(1-y)}}}$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Bernoulli \\begin{aligned}
Y_i &\\sim \\text{Bernoulli}(\\pi_i) \\\\
\\pi_i &= \\pi  \\\\
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")

  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $${ L(\\pi|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\pi^{y_i}(1-\\pi)^{{(1-y_i)}}}$$
                Log Likelihood: $${ \\ln[L(\\pi|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln(\\pi) }$$ $${   + \\sum_{i=1}^{n} (1-y_i) \\ln(1-\\pi)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}



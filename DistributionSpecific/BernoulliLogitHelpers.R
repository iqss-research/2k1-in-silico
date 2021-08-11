

bernLogitSlider <- sliderInput("param1",
                          div(HTML("Choose &beta;:")),
                          min = -3,
                          max = 3,
                          value = 1.2,
                          step = .1)


bernLogitPlotDistr <- function(param, xRow=1){
  param <- param[1]
  paramTransform <- 1/(1 + exp(-param))
  
  analyticalDistr <- data.frame(
    drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Successes (1)", "Failures (0)")),
    prob = c(paramTransform, 1-paramTransform)
  )
  
  ret <- binaryDistrPlotter(analyticalDistr, paramTransform, "\\pi", roundDigits = 2)
  
  ret
}


bernLogitDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){param <- 1}
  paramTransform <- 1/(1 + exp(-param))
  
  random <- runif(nObs) # n i.i.d. uniform draws
  outcome <- ifelse(random <= paramTransform, 1, 0) # how many < pi
  
  return(outcome)
}

# Function mapping parameters pi to likelihood
bernLogitLikelihoodFun <- function(testParam, outcome){
  
  paramTransform <- 1/(1 + exp(-testParam))
  
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  log((paramTransform^(nSuccesses))*((1-paramTransform)^(nObs - nSuccesses)))
}

singleChartDomain <- seq(-5,5,.01)
bernLogitChartDomain <- expand.grid(singleChartDomain)


bernLogitLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large  P(y|\\pi) = \\pi^y(1-\\pi)^{{(1-y)}} \\quad \\text{where} \\quad \\pi =  \\frac{{1}}{{1 + \\text{exp}(-\\beta)}} }$$")

  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Bernoulli \\begin{aligned}
Y_i &\\sim \\text{Bernoulli}\\left ( \\pi \\right) \\\\
\\pi_i &= 1/(1 + \\text{exp}(-\\beta))  \\\\
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $${  L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\left ( \\frac{{1}}{{1 + \\text{exp}(-\\beta)}}\\right)^{y_i} }$$ $${\\cdot \\left(  \\frac{{\\text{exp}(-\\beta)}}{{1 + \\text{exp}(-\\beta)}} \\right )^{{(1-y_i)}}}$$
               Log Likelihood: $$ \\ln[L(\\beta|y)] \\dot{=}  { -\\sum_{i=1}^{n} \\ln(1+ \\text{exp}(-\\beta[1-2y_i])) }$$")

  } else stop("Unknown Markdown!")
  
  
}





bernLogitSlider <- sliderInput("param",
                          "Set Parameter Beta:",
                          min = -3,
                          max = 3,
                          value = 1.2,
                          step = .1)


bernLogitPlotDistr <- function(param){
  
  paramTransform <- 1/(1 + exp(-param))
  
  analyticalDistr <- data.frame(
    drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Successes (1)", "Failures (0)")),
    prob = c(paramTransform, 1-paramTransform)
  )
  
  ggplot(analyticalDistr, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity") +
    scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
    labs(x= "y", y = "P(y|Beta)")+
    theme_minimal() +
    ylim(0,1) +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotation_custom(
      grobTree(textGrob(paste0("Beta: ", sprintf("%0.2f", param)),
                        x=0.7,  y=.95, hjust=0,
                        gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    )
}


bernLogitDraws <- function(param, nObs){
  
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

bernLogitChartDomain <- (-500:500)/100


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
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $${ P(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\left ( \\frac{{1}}{{1 + \\text{exp}(-\\beta)}}\\right)^{y_i} }$$ $${\\cdot \\left(  \\frac{{\\text{exp}(-\\beta)}}{{1 + \\text{exp}(-\\beta)}} \\right )^{{(1-y_i)}}}$$
               Log Likelihood: $${ \\ln[P(\\beta|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln \\left(\\frac{{1}}{{1 + \\text{exp}(-\\beta)}} \\right) }$$ $${   + \\sum_{i=1}^{n} (1-y_i) \\ln \\left( 1-\\frac{{1}}{{1 + \\text{exp}(-\\beta)}} \\right)}$$")

  } else stop("Unknown Markdown!")
  
  
}




bernParamDefault <- .3

bernSlider <- sliderInput("param",
              "Set Parameter Pi:",
              min = 0,
              max = 1,
              value = bernParamDefault,
              step = .1)


bernPlotDistr <- function(param){
  
  if(param>1){param <- 1}
  
  analyticalDistr <- data.frame(
    drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Successes (1)", "Failures (0)")),
    prob = c(param, 1-param)
  )
  
  ggplot(analyticalDistr, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity") +
    scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
    labs(x= "y", y = "P(y|pi)")+
    theme_minimal() +
    ylim(0,1) +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotation_custom(
      grobTree(textGrob(paste0("Pi: ", sprintf("%0.2f", param)),
                        x=0.7,  y=.95, hjust=0,
                        gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    )
}


bernDraws <- function(param, nObs){
  
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

bernChartDomain <- (1:100)/100


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
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $${ P(\\pi|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\pi^{y_i}(1-\\pi)^{{(1-y_i)}}}$$
                Log Likelihood: $${ \\ln[P(\\pi|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln(\\pi) }$$ $${   + \\sum_{i=1}^{n} (1-y_i) \\ln(1-\\pi)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}



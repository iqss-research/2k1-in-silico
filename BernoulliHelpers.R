bernDraws <- function(piParam, nTrials){
  
  random <- runif(nTrials) # n i.i.d. uniform draws
  outcome <- ifelse(random <= piParam, 1, 0) # how many < pi
  
  return(outcome)
}

# Function mapping parameters pi to likelihood
bernLikelihoodFun <- function(testParam, nSuccesses, nObs){log((testParam^(nSuccesses))*((1-testParam)^(nObs - nSuccesses)))}

bernMLE <- function(outcome, intervals = 20){
  
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  
  testPiParam <- (1:intervals)/intervals
  probOutcomeGivenPi <- bernLikelihoodFun(testPiParam, nSuccesses, nObs)
  
  return <- data.frame(Pi = testPiParam, LogLikelihood = probOutcomeGivenPi)
  
}


bernDataPrintHelper <- function(header, bernData, printLength = 25){
  
  message(header, appendLF = TRUE)
  lapply(bernData[1:min(printLength, length(bernData))], function(a){
    message(a)
    Sys.sleep(0.00001)
  })
  if(length(bernData) > printLength){message("...")}
  
}

bernPlotDistr <- function(piParam){
  
  analyticalDistr <- data.frame(
    drawVal = factor(c("Successes (1)", "Failures (0)"), levels = c("Successes (1)", "Failures (0)")),
    prob = c(piParam, 1-piParam)
  )
  
  ggplot(analyticalDistr, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity") +
    scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
    labs(x= "y", y = "P(y|pi)")+
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    )
}


bernPlotMLE <- function(outcome){
  
  chartLen <- 100
  chartDomain <- 1:chartLen/chartLen
  
  likelihoodDB <- bernMLE(outcome = outcome, intervals = chartLen)
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  
  qApprox <- quadraticLikelihoodApprox(
    likelihoodFun = bernLikelihoodFun,
    chartDomain = chartDomain, testParams = .5, nSuccesses = nSuccesses, nObs = nObs)
  likelihoodDB <- likelihoodDB %>%  left_join(qApprox$data, by = c("Pi" = "param") ) 
  
  
  labelLLY <- max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)][.1*chartLen])/max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)])), .15)
  labelQAY <- max(abs(likelihoodDB$QuadraticApprox[.1*chartLen])/max(abs(likelihoodDB$QuadraticApprox)), .15)
  
  if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelLLY <- labelQAY - .1}
  if((labelLLY - labelQAY < 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
  
  grob1 <- grobTree(textGrob(paste0("Log Likelihood - MLE ", sprintf("%0.2f", qApprox$paramHat)),
                             x=0.05,  y=1-labelLLY, hjust=0, gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
  grob2 <- grobTree(textGrob(paste0("Quadratic Approximation - SE: ", sprintf("%0.2f", qApprox$paramSE)),
                             x=0.05,  y=1-labelQAY, hjust=0, gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
  
    ggplot() + 
    geom_line(data = likelihoodDB, mapping =  aes(x = Pi, y = LogLikelihood), color = "steelblue", size = 1) + 
    geom_line(data = likelihoodDB, mapping =  aes(x = Pi, y = QuadraticApprox), color = "firebrick4", size = 1) +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotation_custom(grob1)+ annotation_custom(grob2)
  
}



bernMarkdown <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\pi) = \\pi^y(1-\\pi)^{{(1-y)}}}$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("\\begin{aligned}
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

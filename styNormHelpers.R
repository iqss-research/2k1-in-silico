styNormSlider <- sliderInput("param",
                            "Set Parameter Beta:",
                            min = -2,
                            max = 2,
                            value = 1,
                            step = .25)

styNormDraws <- function(param, nObs){
  
  random1 <- runif(nObs)
  random2 <- runif(nObs)
  
  draws <- sqrt(-2*log(random1))*cos(2*pi*random2) + param
  
  # hist(draws, breaks = -400:400/100+param)
  
}

styNormLikelihoodFun <- function(testParam, outcome){(-1/2)*sum((outcome-testParam)^2)}

styNormMLE <- function(outcome, testDomain){
  
  probOutcomeGivenBeta <- sapply(X = testDomain,FUN =  function(a) styNormLikelihoodFun(a, outcome))
  
  return <- data.frame(beta = testDomain, LogLikelihood = probOutcomeGivenBeta)
  
}



styNormPlotDistr <- function(param){
  
  analyticalDistr <- data.frame(
    drawVal = -300:300/100 + param
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (drawVal - param)^2))
  
  ggplot(analyticalDistr, aes(x = drawVal, y = prob)) + geom_line() +
    labs(x= "y", y = "P(y|beta)") + 
    xlim(-5,5) +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    )
  
  
}


styNormDataPrintHelper <- function(header, data, printLength){
  
  if(length(data) > printLength){truncData <- data[1:printLength]}
  else{truncData <- data}
  charData <- lapply(truncData, function(s){sprintf("%0.1f",s)}) %>%  unlist()
  
  printstr <- paste(c(header, charData), collapse = ", ")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
}


styNormPlotMLE <- function(outcome){
  
  chartLen <- 100
  chartDomain <- ((-5*chartLen):(5*chartLen))/chartLen
  
  likelihoodDB <- styNormMLE(outcome = outcome, testDomain = chartDomain)
  
  qApprox <- quadraticLikelihoodApprox(
    likelihoodFun = styNormLikelihoodFun,
    chartDomain = chartDomain, testParams = .5, outcome = outcome)
  likelihoodDB <- likelihoodDB %>%  left_join(qApprox$data, by = c("beta" = "param") ) 
  
  betaHat <- likelihoodDB$beta[which(likelihoodDB$LogLikelihood == max(likelihoodDB$LogLikelihood))]
  labelLLY <- max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)][.1*chartLen])/max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)])), .15)
  
  
  ret <- ggplot() + 
    geom_line(data = likelihoodDB, mapping =  aes(x = beta, y = LogLikelihood), color = "steelblue", size = 1) + 
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    )
  
  if(any(!is.na(likelihoodDB$QuadraticApprox))){
    
    labelQAY <- max(abs(likelihoodDB$QuadraticApprox[.1*chartLen])/max(abs(likelihoodDB$QuadraticApprox)), .15)
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood (MLE: ", sprintf("%0.2f", betaHat), ")"),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation (SE: ", sprintf("%0.2f", qApprox$paramSE), ")"),
                               x=0.05,  y=1-labelQAY, hjust=0,
                               gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
    
    ret <- ret + geom_line(data = likelihoodDB, mapping =  aes(x = beta, y = QuadraticApprox), color = "firebrick4", size = 1)  + annotation_custom(grob1)+ annotation_custom(grob2)
    
  } else {

    labelQAY <- .95
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
        
    grob1 <- grobTree(textGrob(paste0("Log Likelihood - MLE ", sprintf("%0.2f", betaHat)),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation Not Found"),
                               x=0.05,  y=1-labelQAY, hjust=0, gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
    ret <- ret + annotation_custom(grob1)+ annotation_custom(grob2)
  }
  
  ret
  
  
}





styNormLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\beta) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y - \\beta)^2}{2} \\right) }$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: \\begin{aligned}
Y_i &\\sim f_{\\text{stn}}(y_i |\\mu_i) \\\\
\\mu_i &= \\beta  \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ P(\\beta|y) = k(y) \\cdot $$ $$\\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\beta)^2}{2} \\right) $$
                Log Likelihood: $${\\ln[P(\\beta|y)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - \\beta)^2 }$$")
    
  } else stop("Unknown Markdown!")
  
  
}
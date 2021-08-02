multiNormSlider <- column(12,
  sliderInput("param1",
              "Set Parameters Beta:",
              min = -2,
              max = 2,
              value = 1,
              step = .25),
  sliderInput("param2",
              NULL,
              min = -2,
              max = 2,
              value = -1,
              step = .25),
  sliderInput("param3",
              NULL,
              min = -2,
              max = 2,
              value = 0,
              step = .25),
)


  
multiNormPlotDistr <- function(param, margNum){
  
  if(is.null(param)){
    ret <- element_blank()
  }
  else{
    
  margParam <- param[margNum]
  
  analyticalDistr <- data.frame(
    drawVal = -300:300/100 + margParam
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (drawVal - margParam)^2))
  
  ret <- ggplot(analyticalDistr, aes(x = drawVal, y = prob)) + geom_line(color = "steelblue" , size = 1) +
    labs(x= "y", y = paste0("P(y|beta", margNum,")")) + 
    xlim(-5,5) +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotation_custom(
      grobTree(textGrob(paste0("beta", margNum, ": ", sprintf("%0.2f", margParam)),
                        x=0.7,  y=.95, hjust=0,
                        gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    )
  }
  
  ret
}




multiNormDraws <- function(param, nObs){
  
  dimN <- length(param)
  indepVars <- indepVarsBase[1:nObs,1:dimN]
  outcome <- rnorm(nObs, indepVars %*% param,1)
  
  return(outcome)
}

multiNormLikelihoodFun <- function(testParam, outcome){
  
  dimN <- length(testParam)
  indepVars <- indepVarsBase[1:length(outcome),1:dimN]
  (-1/2)*sum((outcome-(indepVars %*% testParam))^2)
  
}


singleChartDomain <- seq(from = -5, to = 5, by = .05 )
multiNormChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)
  



multiNormLatex <- function(type){
  
  if(type == "Distr"){
    
    div(
    withMathJax("$${\\large P(y_i|\\beta) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y - \\mu_i)^2}{2} \\right) }$$
                $$\\text{where} \\quad \\mu_i = X_i \\beta = \\beta_0 + \\beta_1 X_{i,1} + \\beta_2 X_{i,2} $$"),
    tags$small("with X fixed: see", tags$a("Notation", onclick="customHref('Notation')"))
    )
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Stylized Normal \\begin{aligned}
Y_i &\\sim f_{\\text{stn}}(y_i |\\mu_i) \\\\
\\mu_i &= X_i \\beta   \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\; \\;|X \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$  L(\\beta|y, X)= k(y) \\cdot $$ $$\\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - X_i\\beta)^2}{2} \\right) $$
                Log Likelihood: $${\\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - X_i\\beta)^2 }$$")
    
  } else stop("Unknown Markdown!")
  
  
}
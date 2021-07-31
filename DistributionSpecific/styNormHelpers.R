styNormSlider <- sliderInput("param1",
                            "Set Parameter Beta:",
                            min = -2,
                            max = 2,
                            value = 1,
                            step = .25)

styNormPlotDistr <- function(param, margNum){
  param <- param[1]
  
  analyticalDistr <- data.frame(
    drawVal = -300:300/100 + param
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (drawVal - param)^2))
  
  ggplot(analyticalDistr, aes(x = drawVal, y = prob)) + geom_line(color = "steelblue" , size = 1) +
    labs(x= "y", y = "P(y|beta)") + 
    xlim(-5,5) +
    theme_minimal() +
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




styNormDraws <- function(param, nObs){
  
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
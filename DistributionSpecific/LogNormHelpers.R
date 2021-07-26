logNormSlider <- sliderInput("param",
                             "Set Parameter Beta:",
                             min = -1,
                             max = 2,
                             value = 1,
                             step = .25)

logNormPlotDistr <- function(param){
  
  analyticalDistr <- data.frame(
    drawVal = 1:5000/500
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = exp(-(1/2)*(log(drawVal) - param)^2 )/(drawVal*sqrt(2*pi)))
  

  
  ggplot(analyticalDistr, aes(x = drawVal, y = prob)) + geom_line(color = "steelblue" , size = 1) +
    labs(x= "y", y = "P(y|beta)") + 
    xlim(0.001,10) +
    ylim(0,2)+
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

logNormDraws <- function(param, nObs){rlnorm(1:nObs, param)}

logNormLikelihoodFun <- function(testParam, outcome){(-1/2)*sum((log(outcome)-testParam)^2)}

logNormChartDomain <- ((-2*100):(2*100))/100

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
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ P(\\beta|y) = k(y) \\cdot $$ $$\\prod_{i = 1}^{n}(y_i\\sqrt{2\\pi})^{-1} \\text{exp} \\left( -\\frac{(\\ln (y_i) - \\beta)^2}{2} \\right) $$
                Log Likelihood: $${\\ln[P(\\beta|y)] \\, \\dot{=}\\,-\\frac{1}{2} \\sum_{i=1}^{n}  (\\ln (y_i) - \\beta)^2  }$$")
    
  } else stop("Unknown Markdown!")
  
}
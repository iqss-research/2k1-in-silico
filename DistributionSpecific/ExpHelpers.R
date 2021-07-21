expSlider <- sliderInput("param",
                             "Set Parameter Lambda:",
                             min = 0,
                             max = 2,
                             value = .25,
                             step = .25)

expPlotDistr <- function(param){
  
  analyticalDistr <- data.frame(
    drawVal = 0:500/100
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = param*exp(-drawVal*param))
  
  ggplot(analyticalDistr, aes(x = drawVal, y = prob)) + geom_line(color = "steelblue" , size = 1, na.rm = T) +
    labs(x= "y", y = "P(y|lambda)") + 
    xlim(0,5) +
    ylim(0,1)+
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotation_custom(
      grobTree(textGrob(paste0("Lambda: ", sprintf("%0.2f", param)),
                        x=0.65,  y=.95, hjust=0,
                        gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    )
  
  
}

expDraws <- function(lambdaParam, nObs){
  if(is.null(lambdaParam)){ lambdaParam <- .25}
  rexp(1:nObs, lambdaParam)}

expLikelihoodFun <- function(testParam, outcome){sum(log(testParam) - testParam*outcome)}

expChartDomain <- (1:200)/100


expLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\lambda \\exp(-\\lambda y)  }$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: \\begin{aligned}
Y_i &\\sim \\text{Exponential}(\\lambda_i) \\\\
\\lambda_i &= \\lambda  \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ P(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n}  \\lambda \\exp(-\\lambda y_i)  $$
                Log Likelihood: $${\\ln[P(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} (\\ln (\\lambda)  - \\lambda y_i)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
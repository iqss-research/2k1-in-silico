expSlider <- sliderInput("param1",
                         div(HTML("Choose &lambda;:")),
                         min = 0,
                         max = 2,
                         value = .25,
                         step = .25)

expPlotDistr <- function(param, xRow=1){
  param <- param[1]
  
  analyticalDistr <- data.frame(
    drawVal = 0:500/100
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = param*exp(-drawVal*param))
  
  continuousDistrPlotter(analyticalDistr, param, '\\lambda', roundDigits = 2, arrow = FALSE)
  
  # ggplot(analyticalDistr, aes(x = drawVal, y = prob)) + geom_line(color = "steelblue" , size = 1, na.rm = T) +
  #   labs(x= "y", y = "P(y|lambda)") + 
  #   xlim(0,5) +
  #   ylim(0,1)+
  #   theme_minimal() +
  #   theme(text = element_text(family = "sans"),
  #         legend.position = "none",  
  #         axis.text.x = element_text(size = 15),
  #         axis.text.y = element_text(size = 15),
  #         axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
  #         axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
  #   ) + annotation_custom(
  #     grobTree(textGrob(paste0("Lambda: ", sprintf("%0.2f", param)),
  #                       x=0.65,  y=.95, hjust=0,
  #                       gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
  #   )
  # 
  
}

expDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- .25} # here to stop an annoying warning
  rexp(1:nObs, param)}

expLikelihoodFun <- function(testParam, outcome){sum(log(testParam) - testParam*outcome)}

singleChartDomain <- seq(.01,2,.01)
expChartDomain <- expand.grid(singleChartDomain)

expLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\lambda \\exp(-\\lambda y)  }$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Exponential \\begin{aligned}
Y_i &\\sim \\text{Exponential}(\\lambda_i) \\\\
\\lambda_i &= \\lambda  \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n}  \\lambda \\exp(-\\lambda y_i)  $$
                Log Likelihood: $${\\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} (\\ln (\\lambda)  - \\lambda y_i)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
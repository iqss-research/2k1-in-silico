expExpSlider <- sliderInput("param1",
                            div(HTML("Choose &beta;:")),
                            min = -2,
                            max = 2,
                            value = .25,
                            step = .25)

expExpPlotDistr<- function(param, xRow=1){
  param <- param[1]
  paramTransform <- exp(-param)
  
  analyticalDistr <- data.frame(
    drawVal = 0:500/100
  )
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = paramTransform*exp(-drawVal*paramTransform))
  
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
      grobTree(textGrob(paste0("Beta: ", sprintf("%0.2f", param)),
                        x=0.65,  y=.95, hjust=0,
                        gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    )
  
  
}

expExpDraws <- function(param, nObs){
  param <- param[1]
  if(is.null(param)){ param <- .25} # here to stop an annoying warning
  paramTransform <- exp(-param)
  rexp(1:nObs, paramTransform)}

expExpLikelihoodFun <- function(testParam, outcome){
  paramTransform <- exp(-testParam)
  
  sum(log(paramTransform) - paramTransform*outcome)}

singleChartDomain <- seq(-2,2,.01)
expExpChartDomain <- expand.grid(singleChartDomain)


expExpLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\lambda \\exp(-\\lambda y)  \\quad \\text{where} \\quad \\lambda = \\text{exp}(-\\beta) }$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Exponential \\begin{aligned}
Y_i &\\sim \\text{Exponential}(\\lambda_i) \\\\
\\lambda_i &= \\text{exp}(-\\beta) \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ L(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\text{exp}(-\\beta) \\exp(-\\text{exp}(-\\beta) y_i)  $$
                Log Likelihood: $${\\ln[ L(\\beta|y)] \\, \\dot{=}\\, -\\sum_{i=1}^{n} (\\beta + \\text{exp}(-\\beta) y_i)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
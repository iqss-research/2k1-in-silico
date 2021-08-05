

poisSlider <- sliderInput("param1",
                          div(HTML("Choose &lambda;:")),
                          min = 1,
                          max = 10,
                          value = 2,
                          step = 1)


poisPlotDistr <- function(param, xRow=1){
  
  param <- param[1]
  
  analyticalDistr <- data.frame(drawVal = 1:20)
  
  analyticalDistr <- analyticalDistr %>%  mutate(prob = (param^drawVal)*exp(-param)/(factorial(drawVal)))
  
  ggplot(analyticalDistr, aes(x = drawVal, y = prob)) +
    geom_point(color = "steelblue",  size = 4, shape = "square") +
    geom_line(color = "steelblue", size = 1) +
    labs(x= "y", y = "P(y|lambda)") +
    ylim(0,.4) +
    xlim(0,20) +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotation_custom(
      grobTree(textGrob(paste0("Lambda: ", sprintf("%d", param)),
                        x=0.65,  y=.95, hjust=0,
                        gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    )
}

poisDraws <- function(param, nObs){
  param <- param[1]
  
  if(param<0){param <- 0}
  rpois(1:nObs, param)
  
  }

poisLikelihoodFun <- function(testParam, outcome){sum(outcome * log(testParam) - testParam)}

singleChartDomain <- seq(.1,10,.1)
poisChartDomain <- expand.grid(singleChartDomain)

poisLatex <- function(type){
  
  if(type == "Distr"){
    
    withMathJax("$${\\large P(y|\\lambda) =  \\frac{\\lambda^y  \\exp(-\\lambda)}{y!}  }$$")
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Poisson \\begin{aligned}
Y_i &\\sim \\text{Poisson}(\\lambda_i) \\\\
\\lambda_i &= \\lambda  \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$ L(\\lambda|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\frac{\\lambda^{y_i}  \\exp(-\\lambda)}{y_i!}  $$
                Log Likelihood: $${\\ln[L(\\lambda|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  \\ln(\\lambda)  - \\lambda \\right)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
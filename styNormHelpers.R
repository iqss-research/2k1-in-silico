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

data <- drawSwitcher("Stylized Normal", param = 2, nObs = 200)

styNormDataPrintHelper <- function(header, data, printLength){
  
  charData <- lapply(data, function(s){sprintf("%0.1f",s)}) %>%  unlist()
  
  printstr <- paste(c(header, charData), collapse = " ")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
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
                Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $${ P(\\beta|y) = k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\beta)^2}{2} \\right) }$$
                Log Likelihood: $${\\ln[P(\\beta|y)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - \\beta)^2 }$$")
    
  } else stop("Unknown Markdown!")
  
  
}
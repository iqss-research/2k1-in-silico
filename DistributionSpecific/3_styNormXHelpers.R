styNormXSlider <- column(12,
  sliderInput("param1",
              label = div(HTML("Choose &beta;<sub>0</sub>:")),
              min = -2,
              max = 2,
              value = 1,
              step = .25),
  sliderInput("param2",
              div(HTML("&beta;<sub>1</sub>:")),
              min = -2,
              max = 2,
              value = -1,
              step = .25),
  sliderInput("param3",
              div(HTML("&beta;<sub>2</sub>:")),
              min = -2,
              max = 2,
              value = 0,
              step = .25),
  tags$p("Choose Observation"),
  fluidRow(column(width = 5, selectInput(inputId = "xRow",
              label = NULL,
              choices = 1:200,
              selected = 1,
              width = "100px")),
  column(width = 7, tags$div(id = 'placeholder')))
)



styNormXParamTransform <- function(p,xVals){
  if(length(p)!=length(xVals)){ return(1)}
  as.numeric(xVals %*% c(p))}



  
styNormXPlotDistr <- function(param){
  
  if(is.null(param)){ret <- element_blank()}
  else{
    
  analyticalDistr <- data.frame(drawVal = seq(-3,3,.01) + param) %>% 
    mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (drawVal - param)^2))
  
  ret <- continuousDistrPlotter(analyticalDistr, param, "\\beta", annotationX = param)
  
  }
  
  ret
}




styNormXDraws <- styNormDraws

styNormXLikelihoodFun <- function(testParam, outcome){
  
  nParams <- length(testParam)
  indepVars <- indepVarsBase[1:length(outcome),1:nParams]
  (-1/2)*sum((outcome-(indepVars %*% testParam))^2)
  
}


singleChartDomain <- seq(from = -5, to = 5, by = .05 )
styNormXChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)
  



styNormXLatex <- function(type){
  
  if(type == "Distr"){
    
    div(
    withMathJax("$${\\large P(y_i|\\beta) = (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - \\mu_i)^2}{2} \\right) }$$
                $$\\text{where} \\quad \\mu_i = X_i \\beta = \\beta_0 + \\beta_1 X_{i,1} + \\beta_2 X_{i,2} $$"),
    tags$small("with X fixed: see", tags$a("Notation", onclick="customHref('Notation')"))
    )
  } else if(type == "Model"){
    
    div(tags$p(withMathJax("Statistical Model: Stylized Normal")),
        tags$p("\\( \\hspace{30px} Y_i \\sim f_{stn}(\\mu_i) \\)"),
        tags$p("\\( \\hspace{30px} \\mu_i = X_i \\beta\\)"),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))
    
  } else if(type == "Likelihood"){
    
    div(tags$p(withMathJax("Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :")),
        tags$p(" \\(\\hspace{30px} {\\small L(\\beta|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi)^{-1/2} \\text{exp} \\left( \\frac{(y_i - X_i\\beta)^2}{2} \\right) } \\)"),
        tags$p("Log Likelihood:"),
        tags$p("\\(\\hspace{30px}\\ln[ L(\\beta|y, X)] \\, \\dot{=}\\, -\\frac{1}{2} \\sum_{i=1}^{n} (y_i - X_i\\beta)^2   \\)"))
  } else stop("Unknown Markdown!")  
  
}
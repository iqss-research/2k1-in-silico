##### WORK IN PROGRESS ######

############################################################
# This template describes the functions you need to 
# define in order to create a new distribution. 
# The function definitions should stay the same.
# 
# To add a new distribution, first add it to DistrNames.xlsx.
# Then make a copy of this file. Fill out the function logic, 
# replacing **** with the  shortName of your distribution.
#
# Once you're finished, put it in the 
# DistributionSpecific folder
#
############################################################

# The UI element that lets a user pick parameters. 
# Use the "label" field to specify your parameter names. 
# If you have less than 3 parameters, delete the irrelevant sliders.
# If you have only 1 parameter, delete the last two elements (that let you choose an X)
***Slider <- column(12,
                         sliderInput("param1",
                                     label = div(HTML("FILL THIS IN")),
                                     min = -.5,
                                     max = .5,
                                     value = .2,
                                     step = .1),
                         sliderInput("param2",
                                     div(HTML("FILL THIS IN")),
                                     min = -.5,
                                     max = .5,
                                     value = 0,
                                     step = .1),
                         sliderInput("param3",
                                     div(HTML("FILL THIS IN")),
                                     min = -.5,
                                     max = .5,
                                     value = -.2,
                                     step = .1),
                         tags$p("Choose Observation"),
                         fluidRow(column(width = 5, selectInput(inputId = "xRow",
                                                                label = NULL,
                                                                choices = 1:200,
                                                                selected = 1,
                                                                width = "100px")),
                                  column(width = 7, tags$div(id = 'placeholder')))
)


# This function creates the first plot, the analytical distribution.
# It needs no input besides a vector of parameters, and the 
# row of X you're using, where applicable.
***PlotDistr <- function(param, xRow = 1){
  
  if(is.null(param) || is.null(xRow)){ret <- element_blank()}
  else{
    
    nParams <- length(param)
    xVals <- indepVarsBase[xRow, 1:nParams]
    
    paramTransform <- exp(as.numeric(xVals %*% c(param)))
    
    analyticalDistr <- data.frame(drawVal = seq(0,30,2))
    
    analyticalDistr <- analyticalDistr %>%  mutate(prob = (paramTransform^drawVal)*exp(-paramTransform)/(factorial(drawVal)))
    
    ret <- continuousDistrPlotter(analyticalDistr, paramTransform, '\\lambda', roundDigits = 2, arrow = FALSE, discreteOutput = TRUE)
    
  }
  
  ret
}


***Draws <- function(param, nObs, xRow = 1){
  
  nParams <- length(param)
  indepVars <- indepVarsBase[xRow:nObs,1:nParams]
  paramTransform <- exp(indepVars %*% param)
  outcome <- rpois(1:nObs, paramTransform)
  
  return(outcome)
}

***LikelihoodFun <- function(testParam, outcome){
  
  nParams <- length(testParam)
  indepVars <- indepVarsBase[1:length(outcome),1:nParams]
  
  paramTransform <- exp(indepVars %*% testParam)
  sum(outcome * log(paramTransform) - paramTransform)
  
}


singleChartDomain <- seq(from = -.5, to = .5, by = .01 )
***ChartDomain <- 
  expand.grid(
    singleChartDomain,
    singleChartDomain,
    singleChartDomain)




***Latex <- function(type){
  
  if(type == "Distr"){
    
    div(
      withMathJax("$${\\large P(y_i |\\beta) =  \\frac{\\lambda_i^{y_i}  \\exp(-\\lambda_i)}{y_i !} }$$
                $$\\text{where} \\quad \\lambda_i = \\exp(X_i \\beta) = \\beta_0 + \\beta_1 X_{i,1} + \\beta_2 X_{i,2} $$"),
      tags$small("with X fixed: see", tags$a("Notation", onclick="customHref('Notation')"))
    )
    
  }
  else if(type == "Model"){
    
    withMathJax("Statistical Model: Stylized Normal \\begin{aligned}
Y_i &\\sim \\text{Poisson}(\\lambda_i) \\\\
\\lambda_i &= \\text{exp}(X_i \\beta)   \\\\  
Y_i &\\perp \\!\\!\\! \\perp Y_j \\; \\;|X \\quad \\forall \\: i \\neq j \\\\
\\end{aligned}")
    
    
  } else if(type == "Likelihood"){
    
    withMathJax("
     Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :  $$  L(\\beta|y) = k(y) \\cdot $$
                $$\\prod_{i = 1}^{n} \\frac{\\text{exp}(X_i\\beta)^{y_i}  \\text{exp}(-\\text{exp}(X_i\\beta))}{y_i!}  $$
                Log Likelihood: $${\\ln[ L(\\beta|y)] \\, \\dot{=}\\, \\sum_{i=1}^{n} \\left(y_i  X_i\\beta  - \\text{exp}(X_i\\beta) \\right)}$$")
    
  } else stop("Unknown Markdown!")
  
  
}
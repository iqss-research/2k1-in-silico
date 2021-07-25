

############################################################
# Generic Helpers
############################################################



in_silence <- function(...)
{
  mc <- match.call()[-1]
  a <- capture.output(
    tryCatch(
      suppressMessages(suppressWarnings(
        eval(as.list(mc)[[1]])
      )), error = function(e) ""))
}


quadraticLikelihoodApprox <- function(chartDomain, likelihoodFun, testParams, ...){
  
  
  print(chartDomain[1])
  # in_silence({
    result <- try({
      optimizer <- optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)
      paramHat <- optimizer$par
      paramHessian <- optimizer$hessian
      paramSE <- solve(-1*optimizer$hessian) %>%  sqrt()
      QApprox <-  optimizer$hessian*(chartDomain-paramHat)^2/2 + likelihoodFun(paramHat,...)
      
      list(data = data.frame(param = chartDomain, QuadraticApprox= QApprox), paramHat = paramHat, paramSE = paramSE)
    }, silent = TRUE)
    if (!inherits(result, "try-error")){
      ret <- result
    } else {
      ret <- list(data = data.frame(param = chartDomain, QuadraticApprox= NA), paramHat = NA, paramSE = NA)
    }
    
    
  # })
  
  return(ret)
  
}

generalMleFun <- function(outcome, chartDomain, LikelihoodFun){
  probOutcome <- sapply(X = chartDomain,FUN =  function(a) LikelihoodFun(testParam = a, outcome = outcome))
  
  return <- data.frame(param = chartDomain, LogLikelihood = probOutcome)
}


MLEPlotter <- function(outcome, chartDomain, LikelihoodFun, paramName = ""){
  
  xAxisName <- paste0("Parameter ", paramName)
  
  likelihoodDB <- generalMleFun(outcome,chartDomain, LikelihoodFun)
  chartLen <- length(chartDomain)
  
  qApprox <- quadraticLikelihoodApprox(
    likelihoodFun = LikelihoodFun,
    chartDomain = chartDomain, testParams = .5, outcome = outcome)
  likelihoodDB <- likelihoodDB %>%  left_join(qApprox$data, by = c("param" = "param") ) 
  
  paramHat <- likelihoodDB$param[which(likelihoodDB$LogLikelihood == max(likelihoodDB$LogLikelihood))]
  labelLLY <- max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)][.1*chartLen])/max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)])), .15)
  
  
  ret <- ggplot() + 
    geom_line(data = likelihoodDB, mapping =  aes(x = param, y = LogLikelihood), color = "steelblue", size = 1) + 
    theme_minimal() +
    xlab(xAxisName) +
    theme(text = element_text(family = "sans"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    )
  
  if(any(!is.na(likelihoodDB$QuadraticApprox))){
    
    labelQAY <- max(abs(likelihoodDB$QuadraticApprox[.1*chartLen])/max(c(abs(likelihoodDB$QuadraticApprox), abs(likelihoodDB$LogLikelihood))), .15)
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood (MLE: ", sprintf("%0.2f", paramHat), ")"),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation (SE: ", sprintf("%0.2f", qApprox$paramSE), ")"),
                               x=0.05,  y=1-labelQAY, hjust=0,
                               gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
    
    ret <- ret + geom_line(data = likelihoodDB, mapping =  aes(x = param, y = QuadraticApprox), color = "firebrick4", size = 1)  + annotation_custom(grob1)+ annotation_custom(grob2)
    
  } else {
    
    labelQAY <- .95
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood - MLE ", sprintf("%0.2f", paramHat)),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation Not Found"),
                               x=0.05,  y=1-labelQAY, hjust=0, gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
    ret <- ret + annotation_custom(grob1)+ annotation_custom(grob2)
  }
  
  ret
  
  
}


decPrintHelper <- function(header, data, printLength){
  
  if(length(data) > printLength){truncData <- data[1:printLength]}
  else{truncData <- data}
  charData <- lapply(truncData, function(s){sprintf("%0.1f",s)}) %>%  unlist()
  
  printstr <- paste(c(charData), collapse = ", ")
  printstr <- paste(header, printstr, sep = "")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
}


intPrintHelper <- function(header, data, printLength = 25){
  
  printstr <- paste(c(header, data), sep = " ")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
}




############################################################
# Mapping distributions to functions to use
############################################################

distrList <- list(
  "Bernoulli",
  "Stylized Normal" ,
  "Poisson",
  "Exponential",
  "Log-Normal",
  "Bernoulli-Logit"
)

sliderList <- list(
  bernSlider,
  styNormSlider,
  poisSlider,
  expSlider,
  logNormSlider,
  bernLogitSlider
)

distrPlotList <- list(
  bernPlotDistr,
  styNormPlotDistr,
  poisPlotDistr,
  expPlotDistr,
  logNormPlotDistr,
  bernLogitPlotDistr
)

MLEList <- list(
  function(a){MLEPlotter(a, bernChartDomain, bernLikelihoodFun, "Pi")},
  function(a){MLEPlotter(a, styNormChartDomain, styNormLikelihoodFun, "Beta")},
  function(a){MLEPlotter(a, poisChartDomain, poisLikelihoodFun, "Lambda")},
  function(a){MLEPlotter(a, expChartDomain, expLikelihoodFun, "Lambda")},
  function(a){MLEPlotter(a, logNormChartDomain, logNormLikelihoodFun, "Beta")},
  function(a){MLEPlotter(a, bernLogitChartDomain, bernLogitLikelihoodFun, "Beta")}
)

dataprintList <- list(
  intPrintHelper,
  decPrintHelper,
  intPrintHelper,
  decPrintHelper,
  decPrintHelper,
  intPrintHelper
)


randomDrawList <- list(
  bernDraws,
  styNormDraws,
  poisDraws,
  expDraws,
  logNormDraws,
  bernLogitDraws
)

latexList <- list(
  bernLatex,
  styNormLatex,
  poisLatex,
  expLatex,
  logNormLatex,
  bernLogitLatex
)


paramSwitcher <- function(distrID){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- sliderList[[idx]]
  return(f)} else(stop("Unknown Distribution!"))
  
}

distrPlot <- function(distrID, ...){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- distrPlotList[[idx]]
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}


MLEPlot <- function(distrID, ...){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- MLEList[[idx]]
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}




dataPrintSwitcher <- function(distrID,...){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- dataprintList[[idx]]
  return(f(...) )} else(stop("Unknown Distribution!"))
  
  
}

drawSwitcher <- function(distrID, ...){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- randomDrawList[[idx]]
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}



latexSwitcher <- function(distrID, ...){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- latexList[[idx]]
  return(f(...) )} else(stop("Unknown Distribution!"))
}





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
  
  in_silence({
    result <- try({
      optimizer <- optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)
      paramHat <- optimizer$par
      paramHessian <- optimizer$hessian
      paramSE <- diag(solve(-1*optimizer$hessian) %>%  sqrt())
      paramHatMatrix <- matrix(rep(paramHat, nrow(chartDomain)), ncol = ncol(chartDomain), byrow = T)
      diffMat <- (chartDomain %>%  as.matrix() )- paramHatMatrix
      
      QApprox <- c()
      for(i in 1:nrow(diffMat)){
        
        tmpVec <- diffMat[i,]
        QApprox <- c(QApprox, .5*(t(tmpVec) %*% optimizer$hessian %*%  tmpVec))
        
      }
      QApprox <- QApprox  + likelihoodFun(paramHat,...)
      
      
      
      
      list(data = data.frame(param = chartDomain, QuadraticApprox= QApprox), paramHat = paramHat, paramSE = paramSE)
    }, silent = TRUE)
    if (!inherits(result, "try-error")){
      ret <- result
    } else {
      ret <- list(data = data.frame(param = chartDomain, QuadraticApprox= NA), paramHat = NA, paramSE = NA)
    }
    
    
  })
  
  return(ret)
  
}

generalMleFun <- function(outcome, chartDomain, likelihoodFun){
  
  LogLikelihood <- c()
  
  for(i in 1:nrow(chartDomain)){
    
    testParam <- chartDomain[i,] %>%  t() %>% as.vector()
    LogLikelihood <- c(LogLikelihood, likelihoodFun(testParam =testParam, outcome = outcome))
    
  }
  
  return(cbind(param = chartDomain, as.data.frame(LogLikelihood)))
}


MLEPlotter <- function(outcome, chartDomain, likelihoodFun, paramName = "", margNum = 1){
  
  if(length(margNum) == 0){margNum <- 1}
  
  xAxisName <- paste0("Parameter ", paramName)
  
  likelihoodAll <- generalMleFun(outcome,chartDomain, likelihoodFun)
  nParam <- ncol(chartDomain)
  
  
  qApprox <- quadraticLikelihoodApprox(
    likelihoodFun = likelihoodFun,
    chartDomain = chartDomain, testParams = rep(.5, nParam), outcome = outcome)
  suppressMessages(likelihoodAll <- likelihoodAll %>%  left_join(qApprox$data))
  
  paramHat <- (likelihoodAll %>%  filter(LogLikelihood == max(LogLikelihood)))[, 1:nParam]
  if(nrow(paramHat) > 1 && !is.null(nrow(paramHat))){paramHat <- paramHat[1,]}
  # fix non-marginals at their MLE
  likelihoodDB <- likelihoodAll
  margRemoveCols <- (1:nParam)[which(1:nParam != margNum)]
  for(j in (1:nParam)[margRemoveCols]){
    likelihoodDB <- likelihoodDB %>%  filter_at(c(j), all_vars(.==paramHat[[j]]))}
  
  if(length(margRemoveCols) > 0){likelihoodDB <- likelihoodDB %>%  select_at(-c(margRemoveCols))}
  
  colnames(likelihoodDB) <- c("param", "LogLikelihood", "QuadraticApprox")
  # charting begins here
  
  
  chartLen <- nrow(likelihoodDB)
  labelLLY <- max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)][quantile(chartLen, .1)])/max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)])), .15)
  
  
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
    
    labelQAY <- max(abs(likelihoodDB$QuadraticApprox[quantile(chartLen, .1)])/max(c(abs(likelihoodDB$QuadraticApprox), abs(likelihoodDB$LogLikelihood))), .15)
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood (MLE: ", sprintf("%0.2f", paramHat), ")"),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation (SE: ", sprintf("%0.2f", qApprox$paramSE[margNum]), ")"),
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


intPrintHelper <- function(header, data, printLength){

  printstr <- paste(c(header, data), sep = " ")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
}

## send a string f that parses to a function. Use ? instead of i. 
## creates this object in the specified environment. Returns nothing. Use for side effects. 
listParser <- function(num, funStr, envToUse){
  
  for(i in 1:num){
    eval(parse(text = gsub("\\?", i, funStr)), envir = envToUse )
    
  }
  
  return(NULL)
}

marginalSelectInput <- function(num, pageNum, session = session){
  
  if(num ==1) {
    shinyjs::hide("marginalSelector")
    ret <- "tags$script('Shiny.setInputValue('marginalSelected', 1)')"
    
    } 
  else{
    ret <- selectInput(
      inputId = paste0("marginalSelected",pageNum), label = "Choose marginal distribution to view", choices = 1:num, selected = 1)
  }
  
  ret
}


############################################################
# Mapping distributions to functions to use
############################################################

distrList <- list(
  "Bernoulli",
  "Bernoulli-Logit",
  "Stylized-Normal" ,
  "Multiparameter-Normal",
  "Log-Normal",
  "Poisson",
  "Poisson-Exponential",
  "Exponential",
  "Exponential-Exponential"
)

distrGroups <- list(
  "Bernoulli",
  "Bernoulli",
  "Normal",
  "Normal",
  "Log-Normal",
  "Poisson",
  "Poisson",
  "Exponential",
  "Exponential"
)

optGroups <- list()

for(g in unique(distrGroups)){
  
  distrs <- distrList[which(distrGroups == g)]
  
  newNames <- c(names(optGroups), g)
  optGroups <- append(optGroups, list(distrs))
  names(optGroups) <- newNames
  
}



sliderList <- list(
  bernSlider,
  bernLogitSlider,
  styNormSlider,
  multiNormSlider,
  logNormSlider,
  poisSlider,
  poisExpSlider,
  expSlider,
  expExpSlider
)


nVarList <- list(
  1,
  1,
  1,
  3,
  1,
  1,
  1,
  1,
  1
)


distrPlotList <- list(
  bernPlotDistr,
  bernLogitPlotDistr,
  styNormPlotDistr,
  multiNormPlotDistr,
  logNormPlotDistr,
  poisPlotDistr,
  poisExpPlotDistr,
  expPlotDistr,
  expExpPlotDistr
)

MLEList <- list(
  function(a, margNum){MLEPlotter(a, bernChartDomain, bernLikelihoodFun, "Pi")},
  function(a, margNum){MLEPlotter(a, bernLogitChartDomain, bernLogitLikelihoodFun, "Beta")},
  function(a, margNum){MLEPlotter(a, styNormChartDomain, styNormLikelihoodFun, "Beta")},
  function(a, margNum){MLEPlotter(a, multiNormChartDomain, multiNormLikelihoodFun, "Beta", margNum)},
  function(a, margNum){MLEPlotter(a, logNormChartDomain, logNormLikelihoodFun, "Beta")},
  function(a, margNum){MLEPlotter(a, poisChartDomain, poisLikelihoodFun, "Lambda")},
  function(a, margNum){MLEPlotter(a, poisExpChartDomain, poisExpLikelihoodFun, "Beta")},
  function(a, margNum){MLEPlotter(a, expChartDomain, expLikelihoodFun, "Lambda")},
  function(a, margNum){MLEPlotter(a, expExpChartDomain, expExpLikelihoodFun, "Beta")}
)

dataprintList <- list(
  intPrintHelper,
  intPrintHelper,
  decPrintHelper,
  decPrintHelper,
  decPrintHelper,
  intPrintHelper,
  intPrintHelper,
  decPrintHelper,
  decPrintHelper
)


randomDrawList <- list(
  bernDraws,
  bernLogitDraws,
  styNormDraws,
  multiNormDraws,
  logNormDraws,
  poisDraws,
  poisExpDraws,
  expDraws,
  expExpDraws
)

latexList <- list(
  bernLatex,
  bernLogitLatex,
  styNormLatex,
  multiNormLatex,
  logNormLatex,
  poisLatex,
  poisExpLatex,
  expLatex,
  expExpLatex
)



nVarSwitcher <- function(distrID){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- nVarList[[idx]]
  return(f)} else(stop("Unknown Distribution!"))
  
}



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



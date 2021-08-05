

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


quadraticLikelihoodApprox <- function(chartDomain, likelihoodFun, testParams, margNum, ...){
  
  in_silence({
  result <- try({
      optimizer <- optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)
      paramHatRaw <- optimizer$par
      paramHessian <- optimizer$hessian
      paramSE <- diag(solve(-1*optimizer$hessian) %>%  sqrt())
      paramHatMatrix <- matrix(rep(paramHatRaw, nrow(chartDomain)), ncol = ncol(chartDomain), byrow = T)
      diffMat <- (chartDomain %>%  as.matrix() )- paramHatMatrix

      
      minIdx <- lapply(seq_len(ncol(diffMat)), function(i) which.min(abs(diffMat[,i]))) %>%  unlist()
      paramHat <- diag(chartDomain[minIdx,] %>%  as.matrix())
      chartDomainSmall <- chartDomain
      
      
      margRemoveCols <- (1:ncol(chartDomainSmall))[which(1:ncol(chartDomainSmall) != margNum)]
      for(j in (1:ncol(chartDomainSmall))[margRemoveCols]){
        chartDomainSmall <- chartDomainSmall %>%  filter_at(c(j), all_vars(.==paramHat[[j]]))}
      
      paramHatMatrixSmall <-  matrix(rep(paramHatRaw, nrow(chartDomainSmall)), ncol = ncol(chartDomainSmall), byrow = T)
      diffMatSmall <- (chartDomainSmall %>%  as.matrix() )- paramHatMatrixSmall
      
      
      
      QApproxNew <- c()
      for(i in 1:nrow(diffMatSmall)){
        
        tmpVec <- diffMatSmall[i,]
        QApproxNew <- c(QApproxNew, .5*(t(tmpVec) %*% optimizer$hessian %*%  tmpVec))
        
      }
      QApproxNew <- QApproxNew  + likelihoodFun(paramHatRaw,...)
      LLNew <- generalMleFun(chartDomainSmall, likelihoodFun, ...) %>%  select(LogLikelihood)
      
      result <- list(data = data.frame(param = chartDomainSmall[,margNum],LogLikelihood = LLNew, QuadraticApprox= QApproxNew), paramHat = paramHat, paramSE = paramSE)
    }, silent = TRUE)
    if (!inherits(result, "try-error")){
      result <- result
    } else {
      result <- list(data = data.frame(param = chartDomainSmall[,margNum],LogLikelihood = LLNew, QuadraticApprox= NA), paramHat = NA, paramSE = NA)
    }


  })

  return(result)
  
}

generalMleFun <- function(chartDomain, likelihoodFun, outcome){
  
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
  nParam <- ncol(chartDomain)
  qApprox <- quadraticLikelihoodApprox(likelihoodFun = likelihoodFun, chartDomain = chartDomain,
                                       testParams = rep(.5, nParam), margNum = margNum, outcome = outcome)
  likelihoodDB <- qApprox$data
  paramHat <- qApprox$paramHat
  
  colnames(likelihoodDB) <- c("param", "LogLikelihood", "QuadraticApprox")
  
  # charting begins here
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
  chartLen <- nrow(likelihoodDB)
  labelLLY <- max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)][quantile(1:chartLen/100, .8)])/max(abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)])), .15)
  labelLLY <- min(labelLLY, .85)
  
  if(any(!is.na(likelihoodDB$QuadraticApprox))){
    
    labelQAY <- max(abs(likelihoodDB$QuadraticApprox[quantile(chartLen, .1)])/max(c(abs(likelihoodDB$QuadraticApprox), abs(likelihoodDB$LogLikelihood))), .15)
    labelQAY <- min(labelQAY, .85)
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood (MLE: ", sprintf("%0.2f", paramHat[margNum]), ")"),
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
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood - MLE ", sprintf("%0.2f", paramHat[margNum])),
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

marginalSelectInput <- function(num, pageNum, choicesInput, session = session){
  
  if(num ==1) {
    shinyjs::hide("marginalSelector")
    ret <- tags$script(paste0("Shiny.setInputValue('marginalSelected'",pageNum, ", 1)")) ### NOT WORKING
    
    } 
  else{
    ret <- selectInput(
      inputId = paste0("marginalSelected",pageNum),
      label = "Choose marginal distribution to view",
      choices = choicesInput, selected = choicesInput[1] )
  }
  
  ret
}


continuousDistrPlotter <- function(distrDF, paramVal, paramTex,
                                   annotate = TRUE,
                                   annotationX = NULL,
                                   arrow = TRUE,
                                   roundDigits = 1,
                                   discreteOutput = FALSE,
                                   plotColor = "steelblue"){
    
  if(is.null(annotationX)){annotationX <- mean(distrDF$drawVal)}
  
  paramVal <- as.numeric(paramVal)
  annotationX <- as.numeric(annotationX)
  
  p <- ggplot(distrDF, aes(x = drawVal, y = prob)) + geom_line(color = plotColor , size = 1) +
    labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
    xlim(min(distrDF$drawVal),max(distrDF$drawVal)) +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5))
  

  if(annotate){p <- p +
    annotate("text", x = annotationX, y = quantile(distrDF$prob,.25),
                             label  = parse(
                               text=TeX(paste0("$",paramTex,"$","=",round(paramVal, 1)), output = "character")),
                             parse = TRUE, color = plotColor)}
  if(arrow){p <- p +
      annotate("segment", x = annotationX, y = quantile(distrDF$prob,.15), xend = annotationX,
               yend = 0, arrow = arrow(length = unit(0.2, "cm")), color = plotColor)}
  
  if(discreteOutput){p <- p + geom_point(color = plotColor,  size = 3, shape = "square")}
    

  return(p)
  
  
}



############################################
# Switchers
############################################

nVarSwitcher <- function(distrID){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- nVarList[[idx]]
  return(f)} else(stop("Unknown Distribution!"))
  
}


marginalsChoicesSwitcher <- function(distrID, ...){
  
  idx <- which(distrList==distrID)
  
  if(length(idx) > 0){f <- marginalsChoicesList[[idx]]
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



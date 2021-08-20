
############################################################
# Functions for MLE estimation
############################################################



likelihoodEstimateFun <- function(chartDomain, likelihoodFun, testParams, margNum, ...){
  
  in_silence({
    
    optimizer <- tryCatch(
      {optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)},
      error = function(e){
        optim(par = rep(.5, length(testParams)), likelihoodFun, hessian = TRUE, control = list(fnscale = -1), ...)
      })
    paramHatRaw <- optimizer$par
    paramVCov <-  try({solve(-1*optimizer$hessian)}, silent = T)
    paramSE <- try({diag(solve(-1*optimizer$hessian) %>%  sqrt())}, silent = T)
    if (!inherits(paramSE, "try-error")){paramSE <- paramSE } else{paramSE <- NA}
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
    
    result <- list(data = data.frame(param = chartDomainSmall[,margNum],LogLikelihood = LLNew, QuadraticApprox= QApproxNew), paramHat = paramHat, paramSE = paramSE, paramVCov = paramVCov)
    
    
    
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


MLEstimator <- function(outcome, chartDomain, likelihoodFun, paramName = "", margNum = 1){
  if(length(margNum) == 0){margNum <- 1}
  
  xAxisName <- paste0("Parameter ", paramName)
  nParam <- ncol(chartDomain)
  qApprox <- likelihoodEstimateFun(likelihoodFun = likelihoodFun, chartDomain = chartDomain,
                                       testParams = rep(0.001, nParam), margNum = margNum, outcome = outcome)
  likelihoodDB <- qApprox$data
  paramHat <- qApprox$paramHat
  
  colnames(likelihoodDB) <- c("param", "LogLikelihood", "QuadraticApprox")
  
  uniqueLL<-sort(unique(likelihoodDB$LogLikelihood))
  maxY <- quantile(likelihoodDB$LogLikelihood,.99)
  maxY <- if(maxY > 0){maxY*1.2}else{maxY * .8}
  
  # charting begins here
  retPlot <- ggplot() + 
    geom_line(data = likelihoodDB, mapping =  aes(x = param, y = LogLikelihood), color = "steelblue", size = 1) + 
    theme_minimal() +
    xlab(xAxisName) +
    ylim(uniqueLL[2],maxY) +
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
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood, MLE: ", sprintf("%0.2f", paramHat[margNum])),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation (from optim), SE: ", sprintf("%0.2f", qApprox$paramSE[margNum])),
                               x=0.05,  y=1-labelQAY, hjust=0,
                               gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
    
    retPlot <- retPlot + geom_line(data = likelihoodDB, mapping =  aes(x = param, y = QuadraticApprox), color = "firebrick4", size = 1)  + annotation_custom(grob1)+ annotation_custom(grob2)
    
  } else {
    
    labelQAY <- .95
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood - MLE ", sprintf("%0.2f", paramHat[margNum])),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approximation Not Found"),
                               x=0.05,  y=1-labelQAY, hjust=0, gp=gpar(col="firebrick4", fontsize=13, fontface="italic")))
    retPlot <- retPlot + annotation_custom(grob1)+ annotation_custom(grob2)
  }
  
  return(list(
    plot = retPlot,
    paramHat = paramHat, 
    paramSE = qApprox$paramSE,
    paramVCov = qApprox$paramVCov
    
  ))
  
  
}



############################################################
# Functions for MLE estimation
############################################################



likelihoodEstimateFun <- function(chartDomain, likelihoodFun, testParams,
                                  margNum, outcome, xVals, optimMethod,
                                  fixValues){
  
  # calls to optim, with error handling
  optimizer <- tryCatch(
    {optim(par = testParams,
           likelihoodFun, hessian = T, control = list(fnscale = -1),
           outcome = outcome, xVals = xVals, method = optimMethod)},
    error = function(e){
      tryCatch({
        optim(par = rep(.25, length(testParams)),
              likelihoodFun, hessian = T, control = list(fnscale = -1),
              outcome = outcome, xVals = xVals, method = optimMethod )},
        error = function(e){return(NULL)})
    })
  if(is.null(optimizer)){return(NULL)}
  # unpack optim results into what we want
  paramHatRaw <- optimizer$par
  paramVCov <-  try({solve(-1*optimizer$hessian)}, silent = T)
  paramSE <- try({diag(solve(-1*optimizer$hessian) %>%  sqrt())}, silent = T)
  if (!inherits(paramSE, "try-error")){paramSE <- paramSE } else{paramSE <- NA}
  nParams <- length(paramHatRaw)
  
  paramHat <- c()
  for(j in 1:nParams){
    dmn <- chartDomain[[j]]
    pValsTmp <- seq(dmn$from, dmn$to, dmn$by)
    if(j == margNum){paramVals <- pValsTmp}
    paramHat[j] <- pValsTmp[which.min(abs(pValsTmp - paramHatRaw[j]))]
    }
  
  chartDomainSmall <- sapply(1:nParams, function(i){
    if(i == margNum){paramVals} else {rep(fixValues[i], length(paramVals))}
  })
  
  paramHatMatrixSmall <-  matrix(rep(paramHatRaw, nrow(chartDomainSmall)), ncol = ncol(chartDomainSmall), byrow = T)
  diffMatSmall <- (chartDomainSmall %>%  as.matrix() )- paramHatMatrixSmall
  
  QApproxNew <- c()
  for(i in 1:nrow(diffMatSmall)){
    
    tmpVec <- diffMatSmall[i,]
    QApproxNew <- c(QApproxNew, .5*(t(tmpVec) %*% optimizer$hessian %*%  tmpVec))
    
  }
  QApproxNew <- QApproxNew  + likelihoodFun(paramHatRaw, outcome = outcome, xVals = xVals)
  LLNew <- generalMleFun(chartDomainSmall, likelihoodFun, outcome = outcome, xVals = xVals) %>%
    dplyr::select(LogLikelihood)
  
  result <- list(data = data.frame(param = chartDomainSmall[,margNum],LogLikelihood = LLNew, QuadraticApprox= QApproxNew), paramHat = paramHat, paramSE = paramSE, paramVCov = paramVCov)
  
  
  return(result)
  
}

generalMleFun <- function(chartDomain, likelihoodFun, outcome, xVals){
  
  LogLikelihood <- c()
  
  for(i in 1:nrow(chartDomain)){
    testParam <- chartDomain[i,] %>%  t() %>% as.vector()
    LogLikelihood <- c(LogLikelihood,
                       likelihoodFun(testParam =testParam, outcome = outcome, xVals = xVals))
  }
  
  return(cbind(param = chartDomain, as.data.frame(LogLikelihood)))
}


MLEstimator <- function(outcome, chartDomain, likelihoodFun, paramName = "", margNum = 1, xVals = matrix(), optimMethod = "Nelder-Mead", fixValues, testParams = NULL){
  
  
  xAxisName <- paste0("Parameter ", paramName)
  nParam <- length(chartDomain)
  
  if(length(margNum) == 0){margNum <- 1}
  if(is.null(testParams)){testParams <- rep(0.01, nParam)}
  
  if(length(fixValues) < nParam){fixValues <- rep(.1,nParam)}
  qApprox <- likelihoodEstimateFun(likelihoodFun = likelihoodFun,
                                   chartDomain = chartDomain,
                                   testParams = testParams,
                                   margNum = margNum,
                                   outcome = outcome,
                                   xVals = xVals,
                                   optimMethod = optimMethod,
                                   fixValues)
  
  if(is.null(qApprox)){return(NULL)}
  likelihoodDB <- qApprox$data
  paramHat <- qApprox$paramHat
  
  colnames(likelihoodDB) <- c("param", "LogLikelihood", "QuadraticApprox")
  
  uniqueLL<-sort(unique(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)]))
  maxY <- quantile(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)],.99)
  minY <- quantile(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)],.01)
  rangeY <- abs(maxY - minY)
  maxY <- if(maxY + .1*rangeY > 0){maxY + .1*rangeY } else {maxY + .1*rangeY }
  
  # TODO: refactor out charting code....
  # charting begins here
  retPlot <- ggplot() + 
    geom_line(data = likelihoodDB, mapping =  aes(x = param, y = LogLikelihood), color = "steelblue", size = 1) + 
    theme_minimal() +
    xlab(xAxisName) +
    ylim(uniqueLL[2],maxY) +
    theme(text = element_text(family = "sans"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    )
  chartLen <- nrow(likelihoodDB)
  labelLLY <- max(abs(
    likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)][quantile(1:chartLen/100, .8)]
  )/max(abs(
    likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)])), .15)
  labelLLY <- min(labelLLY, .85)
  
  if(any(!is.na(likelihoodDB$QuadraticApprox))){
    
    labelQAY <- max(abs(
      likelihoodDB$QuadraticApprox[is.finite(likelihoodDB$QuadraticApprox)][quantile(chartLen, .1)]
    )/max(c(abs(likelihoodDB$QuadraticApprox[is.finite(likelihoodDB$QuadraticApprox)]), abs(likelihoodDB$LogLikelihood[is.finite(likelihoodDB$LogLikelihood)]))), .15)
    labelQAY <- min(labelQAY, .85)
    
    if((labelLLY - labelQAY > 0) && (labelLLY - labelQAY < .1)  ){labelQAY <- labelQAY - .1}
    if((labelLLY - labelQAY <= 0) && (labelLLY - labelQAY > -.1)  ){labelLLY <- labelLLY - .1}
    
    grob1 <- grobTree(textGrob(paste0("Log Likelihood, MLE: ", sprintf("%0.2f", paramHat[margNum])),
                               x=0.05,  y=1-labelLLY, hjust=0,
                               gp=gpar(col="steelblue", fontsize=13, fontface="italic")))
    
    grob2 <- grobTree(textGrob(paste0("Quadratic Approx. (from optim), SE: ", sprintf("%0.2f", qApprox$paramSE[margNum])),
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

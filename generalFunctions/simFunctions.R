############################################################
# Functions to help with simulation
############################################################

paramTildeCreator <- function(paramHat, #\hat{\gamma}
                              paramVCov, #\hat{V}(\hat{\gamma})
                              nSimDraws=1000){
  #get lots of parameters
  paramTilde <- tryCatch({rmvnorm(nSimDraws, paramHat, as.matrix(paramVCov))}, 
                         error = function(e){matrix(rep(NA,nSimDraws*length(paramHat)), nrow = nSimDraws)})
}

intrTildeCreator <- function(paramTilde, transformFun, xVals = c(1)){
  
  intrTilde <- sapply(1:nrow(paramTilde), function(a){transformFun(paramTilde[a,], xVals = c(1,xVals))})
  
  intrTilde <- if(!is.null(dim(intrTilde))){
     intrTilde %>%  t()
  } else {intrTilde}
}

yTildeCreator <- function(intrTilde, #\hat{\mu}
                          model){ # draws function - takes params, returns y
  if(is.null(intrTilde)){return(rep(NA, length(intrTilde)))}
  if(any(lapply(intrTilde,length) > 0)){
    sapply(1:nrow(as.matrix(intrTilde)), function(a){model(as.matrix(intrTilde)[a,] %>%  as.numeric(), 1)})}
  else{
    rep(NA, nrow(as.matrix(intrTilde)))
  }
}

expValCreator <- function(intrTilde,
                          model,
                          nSimDraws=1000){
  if(is.null(intrTilde)){return(rep(NA, length(intrTilde)))}
  intrTildeMat <- as.matrix(intrTilde)
  # probably I can do this with sapply instead
  intrTildeList <- lapply(seq_len(nrow(intrTildeMat)), function(i) intrTildeMat[i,])
  
  if(any(lapply(intrTilde,length) > 0)){
    
    tmp <- lapply(intrTildeList, function(intrTildeVal){
      sapply(1:100, function(a){model(intrTildeVal,1)})
    }) %>%  unlist() %>%  matrix(nrow = nSimDraws)
    rowSums(tmp)/100
  }
  else{
    rep(NA, length(intrTilde))
  }
  
}


QOIVisualization <- function(yTilde, intrTilde, distrConfig, QOIName){
  errMessage <- "Error in computing QOI. Please make sure your simulated \n variables exist, and your Hessian is nonsingular"
  idx <- which(QOIDF$Name==QOIName)
  tryCatch({
    tmpFun <- eval(parse(text=QOIDF$FunctionName[[idx]]))
    tmpFun(yTilde, intrTilde, distrConfig)},error = function(e){
    ggplot() + annotate("text", x = 4, y = 1, size=4, label = paste(errMessage, collapse = " ")) + theme_void()})
  
}


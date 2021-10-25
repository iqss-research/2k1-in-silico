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

muTildeCreator <- function(paramTilde, transformFun, xVals = c(1)){
  
  muTilde <- sapply(1:nrow(paramTilde), function(a){transformFun(paramTilde[a,], xVals = c(1,xVals))})
  
  muTilde <- if(!is.null(dim(muTilde))){
     muTilde %>%  t()
  } else {muTilde}
}

yTildeCreator <- function(muTilde, #\hat{\mu}
                          model){ # draws function - takes params, returns y
  if(is.null(muTilde)){return(rep(NA, length(muTilde)))}
  if(any(lapply(muTilde,length) > 0)){
    sapply(1:nrow(as.matrix(muTilde)), function(a){model(as.matrix(muTilde)[a,] %>%  as.numeric(), 1)})}
  else{
    rep(NA, nrow(as.matrix(muTilde)))
  }
}

expValCreator <- function(muTilde,
                          model,
                          nSimDraws=1000){
  if(is.null(muTilde)){return(rep(NA, length(muTilde)))}
  muTildeMat <- as.matrix(muTilde)
  # probably I can do this with sapply instead
  muTildeList <- lapply(seq_len(nrow(muTildeMat)), function(i) muTildeMat[i,])
  
  if(any(lapply(muTilde,length) > 0)){
    
    tmp <- lapply(muTildeList, function(muTildeVal){
      sapply(1:100, function(a){model(muTildeVal,1)})
    }) %>%  unlist() %>%  matrix(nrow = nSimDraws)
    rowSums(tmp)/100
  }
  else{
    rep(NA, length(muTilde))
  }
  
}


QOIVisualization <- function(yTilde, muTilde, distrID, QOIName){
  errMessage <- "Error in computing QOI. Please make sure your simulated \n variables exist, and your Hessian is nonsingular"
  idx <- which(QOIDF$Name==QOIName)
  tmpFun <- eval(parse(text=QOIDF$FunctionName[[idx]]))
  tryCatch({tmpFun(yTilde, muTilde, distrID)},error = function(e){
    ggplot() + annotate("text", x = 4, y = 1, size=4, label = paste(errMessage, collapse = " ")) + theme_void()})
  
}


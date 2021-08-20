############################################################
# Functions to help with simulation
############################################################

yTildeCreator <- function(paramHat, #\hat{\gamma}
                          paramVCov, #\hat{V}(\hat{\gamma})
                          model, # draws function - takes params, returns y
                          nSimDraws,
                          xRow = 1){ #X_c
  
  #get lots of parameters
  paramTilde <- tryCatch({rmvnorm(nSimDraws, paramHat, as.matrix(paramVCov))}, 
                         error = function(e){
                           matrix(rep(NA,nSimDraws*length(paramHat)), nrow = nSimDraws)
                        })
  
  # \tilde{y}_c 
  yTilde <- sapply(1:nSimDraws, function(a){model(paramTilde[a,], 1, xRow)})
  
}



simHist <- function(yTilde){
  
  if(all(is.na(yTilde))){
     p <- stop("No y data found. Check that your MLE has an invertible Hessian.")
  }
  else{ p <-  tryCatch({
    
    histogramMaker(yTilde, title = "Simulated Y")
  
  },
  error = function(e){stop("Error in drawing histogram. Please refresh or try different parameters.")})
  
  }
  
  return(p)
  
}


QOIVisualization <- function(yTilde, QOIName){
  errMessage <- "Error in computing QOI. Please make sure your simulated variables exist."
  
  ret <- tryCatch({
  idx <- which(QOIDF$Name==QOIName)
  
  if(length(idx) > 0){
    f <- eval(parse(text=QOIDF$FunctionName[[idx]]))
    return(f(yTilde))
  } else{stop("Unknown QOI!")}},
  error = function(e){stop(e)})
  
  ret
  
}

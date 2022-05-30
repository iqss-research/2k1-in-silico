############################################################
# Functions to help with simulation
############################################################

paramTildeCreator <- function(paramHat, #\hat{\gamma}
                              paramVCov, #\hat{V}(\hat{\gamma})
                              nSimDraws=1000){
  #get lots of parameters
  paramTilde <- tryCatch({mvtnorm::rmvnorm(nSimDraws, paramHat, as.matrix(paramVCov))},
                         error = function(e){matrix(rep(NA,nSimDraws*length(paramHat)), nrow = nSimDraws)})
}

intrTildeCreator <- function(paramTilde, transformFun, xVals = c(1)){

  intrTilde <- sapply(1:nrow(paramTilde), function(a){transformFun(paramTilde[a,], xVals = c(xVals), DGP = F)})

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


QOIVisualization <- function(yTilde, intrTilde, distrConfig, QOIName, QOIDF){
  errMessage <- "Error in computing QOI. Please make sure your simulated \n variables exist, and your Hessian is nonsingular"
  idx <- which(QOIDF$Name==QOIName)
  tryCatch({
    tmpFun <- eval(parse(text=QOIDF$FunctionName[[idx]]))
    tmpFun(yTilde, intrTilde, distrConfig)},error = function(e){
      ggplot() + annotate("text", x = 4, y = 1, size=4, label = paste(errMessage, collapse = " ")) + theme_void()})

}

############################################################
# Every function on this page should take one argument,
# y tilde (simulated y).
#
# The function should return a ggplot object and a table
# Either can be empty.
#
# The function name should be listed with the QOI name
# in QOIList.xlsx
############################################################

### TODO clean up args

ycOutput <- function(yTilde, intrTilde, distrConfig){
  ciInt <- if(length(unique(yTilde)) > 2){ quantile(yTilde, c(.1, .9))} else {NULL}

  histogramMaker(yTilde, title = "Predicted Values of Y", annotate = T,
                 ci = ciInt, border = F)}

ycGrtOutput <- function(yTilde, intrTilde, distrConfig){
  histogramMaker(yTilde, title = "Predicted Values of Y", greaterThan = 1)}

paramHistOutput <- function(yTilde, intrTilde, distrConfig){
  if(!is.null(ncol(intrTilde))){intrTilde <- intrTilde[,1]}

  histogramMaker(intrTilde,
                 title = paste0("Simulated Values of Parameter $",distrConfig$intrParamTex,"$"))}


expValsOutput <- function(yTilde, intrTilde, distrConfig){

  expVals <- expValCreator(intrTilde, parser(distrConfig$drawFun))
  xAxis <- distrConfig$simXAxis_param

  histogramMaker(expVals, title = xAxis, annotate = T,
                 ci = quantile(expVals, c(.1, .9)), border = F
  )

}


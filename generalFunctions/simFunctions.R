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
  
  sapply(1:nrow(paramTilde), function(a){transformFun(paramTilde[a,], xVals = c(1,xVals))})
}

yTildeCreator <- function(muTilde, #\hat{\mu}
                          model){ # draws function - takes params, returns y
  if(any(lapply(muTilde,length) > 0)){
    sapply(1:length(muTilde), function(a){model(muTilde[a] %>%  as.numeric(), 1)})}
  else{
    rep(NA, length(muTilde))
  }
  
}

expValCreator <- function(muTilde,
                          model,
                          nSimDraws=1000){
  if(any(lapply(muTilde,length) > 0)){
    tmp <- sapply(1:length(muTilde), function(a){model(muTilde[a] %>%  as.numeric(), nSimDraws)})
    rowSums(tmp)/nSimDraws
    }
  else{
    rep(NA, length(muTilde))
  }
  
  
}


QOIVisualization <- function(yTilde, muTilde, distrID, QOIName){
  errMessage <- "Error in computing QOI. Please make sure your simulated \n variables exist, and your Hessian is nonsingular"
  
  idx <- which(QOIDF$Name==QOIName)
  
  tmpFun <- eval(parse(text=QOIDF$FunctionName[[idx]]))
  tryCatch({tmpFun(yTilde %>%  as.numeric(), muTilde %>%  as.numeric(), distrID)},error = function(e){
    ggplot() + annotate("text", x = 4, y = 1, size=4, label = paste(errMessage, collapse = " ")) + theme_void()})
  
}


############################################################
# simulation LaTeX
############################################################




simMLELatex  <- function(header, matrixData){
  
  tryCatch({
  if(length(matrixData) == 1){
    startTex <- "\\(\\begin{matrix}"
    endTex <- "\\end{matrix} \\)"
  } else {
    startTex <- "\\(\\begin{bmatrix}"
    endTex <- "\\end{bmatrix} \\)"
  }
  
  if(any(!is.null(matrixData))){
  printStr <- paste0(header, startTex)
  rowList <- as.list(data.frame(matrixData %>%  as.matrix())) # relies on symmetry of vcov matrix
  for(r in rowList){
    tmp <- lapply(r, function(s){round(s,2)}) %>%  unlist()
    printStr <- paste0(printStr,paste(tmp, collapse = "&"),"\\\\")
    
  }
  return(withMathJax(paste0(printStr, endTex)))
  } else {return("")}},
  error = function(e){return("No values found. Check that your hessian is nonsingular.")}
  
  )
}



# function making sliders for the sim pages
simMultiSliderFunction <- function(numSliders){
  
  if(numSliders == 0){""} else{
    
    lapply(1:numSliders, function(i){
      sliderInput(
        paste0("simX",i),
        div(HTML(paste0("<p style='color:#ff0000;'><b>X<sub>", i,"</sub></b></p>"))),
        min = -2,
        max = 2,
        value = (-i)^2*.1,
        step = .1,
        width = "75%")
    })}
  
}




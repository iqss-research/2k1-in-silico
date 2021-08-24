############################################################
# Functions to help with simulation
############################################################

paramTildeCreator <- function(paramHat, #\hat{\gamma}
                              paramVCov, #\hat{V}(\hat{\gamma})
                              nSimDraws){
  #get lots of parameters
  paramTilde <- tryCatch({rmvnorm(nSimDraws, paramHat, as.matrix(paramVCov))}, 
                         error = function(e){matrix(rep(NA,nSimDraws*length(paramHat)), nrow = nSimDraws)})
}

muTildeCreator <- function(paramTilde, transformFun, xVals = c(1)){
  
  sapply(1:nrow(paramTilde), function(a){transformFun(paramTilde[a,], xVals = c(1,xVals))})
}

yTildeCreator <- function(muTilde, #\hat{\mu}
                          model){ # draws function - takes params, returns y

  yTilde <- sapply(1:length(muTilde), function(a){model(muTilde[a], 1)})

}


QOIVisualization <- function(yTilde, muTilde, distrID, QOIName){
  errMessage <- "Error in computing QOI. Please make sure your simulated \n variables exist, and your Hessian is nonsingular"
  
  idx <- which(QOIDF$Name==QOIName)
  
  f <- eval(parse(text=QOIDF$FunctionName[[idx]]))
  tryCatch({f(yTilde, muTilde, distrID)},error = function(e){
    ggplot() + annotate("text", x = 4, y = 1, size=4, label = e) + theme_void()})
  
}





############################################################
# simulation LaTeX
############################################################


simMathJax1 <<- 
  div(
    withMathJax("Estimation Uncertainty: \\begin{array} 
                \\, \\tilde{\\theta} \\sim \\mathcal{N}(\\hat{\\theta}, \\hat{V}\\hat{\\theta}) \\\\
                \\, \\{ \\tilde{\\beta}, \\tilde{\\sigma}^2\\} = \\tilde{\\theta}  \\\\
                \\end{array}")
  )

simMathJax2 <<- 
  div(
    withMathJax("Fundamental Uncertainty: \\begin{array} 
                \\, \\tilde{\\mu}_c = X_c \\tilde{\\beta} \\\\
                \\, \\tilde{y}_c  \\sim \\mathcal{N}(\\tilde{\\mu}_c, \\tilde{\\sigma}^2) \\\\
                \\end{array}")
  )

simMathJaxDynamic <- function(xVec){
  
  if(any(!is.null(xVec))){
  allStrs <- paste(lapply(1:length(xVec), function(i){
    paste0(" + \\beta_",i,"( \\color{red}{ ", sprintf("%0.1f", xVec[i]), "})")
  }), collapse = "")} else{allStrs <- ""}
  
  div(
    withMathJax(
      paste0("Fundamental Uncertainty: \\begin{array}",
             "\\, \\tilde{\\mu}_c = X_c \\tilde{\\beta} = \\beta_0", allStrs, "\\\\",
             "\\, \\tilde{y}_c  \\sim \\mathcal{N}(\\tilde{\\mu}_c, \\tilde{\\sigma}^2) \\\\",
             "\\end{array}")
    )
  )
  
  
} 





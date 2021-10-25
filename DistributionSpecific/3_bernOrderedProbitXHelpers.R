######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, exactly two betas and one gamma (plus implicit tau = 0)
nBetas <- 2
machineConst <-  .Machine$double.eps*10

orderedProbitXParamTransform <- function(p,xVals){
  
  betaVals <- p[1:nBetas]
  gammaVals <- p[(nBetas + 1):length(p)]
  if(length(betaVals)!=length(xVals)){ return(1)}
  muParam <- as.numeric(xVals %*% c(betaVals))
  
  tauParams <- Reduce(x = gammaVals, f = function(i,j){
    c(i, tail(i,1) + machineConst + exp(j))}, init = 0) 
  
  return(matrix(c(muParam, tauParams), ncol = length(tauParams)+1, byrow = F))  
  
}

orderedProbitXPDF <- function(drawVal, param){
  muParam <- param[1]
  tauParams <- param[2:length(param)]
  relativeParams <- tauParams - muParam
  
  lowerBound <- if(drawVal >1) {relativeParams[drawVal-1]} else{-9999}
  upperBound <- if(drawVal <= length(relativeParams)) {relativeParams[drawVal]} else{9999}
  
  probitlink(upperBound, inverse = T) - probitlink(lowerBound, inverse = T)
}

orderedProbitXPlotDistr <- function(param, domain, range){
  
  if(is.null(param)){return( element_blank())}
  else{
    
    domain <- seq(domain[1], domain[2])
    probs <- sapply(domain, function(a){
      mean(sapply(1:nrow(param), function(b){orderedProbitXPDF(a, param[b,])}))
      }) 
    distrDF <- data.frame(drawVal = domain, prob = probs)
    
    paramTex <- "\\beta, \\gamma"
    
    ret <- ggplot(distrDF, aes(x = drawVal, y = prob, fill = drawVal)) +
      geom_bar(stat="identity", alpha = .5) +
      scale_fill_gradientn(colors = c("#F8766D", "#7CAE00", "#00BFC4")) +
      labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
      theme_minimal() +
      ylim(0,1.1) +
      theme(text = element_text(family = "sans"),
            legend.position = "none",  
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
            axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5)
      ) 
    
    suppressWarnings({ggplot_build(ret)})
  }
  
}

orderedProbitXDraws <- function(params, nObs){
  
  if(is.null(params)){params <- matrix(rep(1,40), ncol =2)}
  if(is.null(nObs)){nObs <- 20}
  paramMat <- matrix(params, ncol = 3)
  muParam <- paramMat[,1]
  tauParams <- paramMat[,2:ncol(paramMat)]
  probs <- probitlink(cbind(-9999, 
                            matrix(tauParams, ncol = 2),9999), inverse = T)
  
  sapply(1:nrow(paramMat), function(i){
    sample(1:3, prob = diff(probs[i,]), size = 1, replace = TRUE)
  })
  
}

orderedProbitXLikelihoodFun <- function(testParam, outcome, xVals){
  transformedTest <- sapply(1:nrow(xVals), function(i){
                            orderedProbitXParamTransform(p = testParam, xVals = xVals[i,])}) %>%  t()
  vec <- sapply(1:length(outcome), function(i){
    orderedProbitXPDF(drawVal = outcome[i], param = transformedTest[i,])
  } )
  
  sum(log(vec))
  
}


singleChartDomain <- list(from = -5, to = 5, by = .01 )
orderedProbitXChartDomain <- function(n){
  d <- lapply(1:(n+1), function(i){singleChartDomain})
} 


orderedProbitXLatex <- function(type, ...){
  distrLatexFunction(
    type = type, 
    modelName = "Normal",
    pdfTex = "P(y|\\beta, \\tau) = \\Phi^{-1}(\\tau_j-\\mu) - \\Phi^{-1}(\\tau_{j-1}-\\mu)",
    pdfAddendum = 3,
    modelDistTex = " \\mathcal{N}(\\mu_i, \\sigma^2) ",
    modelParamTex = "\\tau_{j-1} \\leq y < \\tau_j,\\quad \\mu_i = X_i \\beta,  \\quad \\tau_i = \\exp(\\gamma_i) ",
    likelihoodTex = " L(\\beta, \\gamma|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} (2\\pi\\exp(\\gamma)^2)^{-1/2} \\text{exp} \\left( \\frac{(y_i - X_i\\beta)^2}{2\\exp(\\gamma)^2} \\right)",
    logLikelihoodTex = "\\ln[ L(\\beta, \\gamma|y, X)] \\, \\dot{=}\\, -n\\gamma -\\frac{1}{2\\exp(\\gamma)^2} \\sum_{i=1}^{n} (y_i - X_i\\beta)^2",
    smallLik = T,
    smallLL = T,
    ...
  )
}
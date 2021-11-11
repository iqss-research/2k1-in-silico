######## NOTE: unlike other distributions, this RELIES on the parameters being in a certain order
######## SPECIFICALLY, exactly two betas and one gamma (plus implicit tau = 0)
# nBetas <- 2
machineConst <-  .Machine$double.eps*10

orderedProbitXParamTransform <- function(p,xVals){
  
  betaVals <- p[1:(length(p)-1)]
  gammaVals <- p[length(p)]
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
      scale_fill_gradientn(colors = cbPalette[1:3]) +
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
  tauParams <- paramMat[,2:ncol(paramMat)] - muParam
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
  d <- lapply(1:n, function(i){singleChartDomain})
} 


orderedProbitXLatex <- function( type, 
                                 nXValsPDF = 1,
                                 nXValsAssumed = 1,
                                 xValsSim = c(),
                                 paramValsPDF = c(),
                                 nParamLL = 0,
                                 paramTex = "",
                                 metaParamTex = "",
                                 smallLik = 0,
                                 smallLL = 0,
                                 nObs = 0, ...){
  if(type == "Distr") {
    
    xStrs <- paste(lapply(1:nXValsPDF, function(i){
      tmpXStr <- if(nXValsPDF > 1){paste0("X_{i,",i,"}")} else {"X_i"}
      paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")
    
    div(
      tags$p(tags$b("Probability Model"), style = "padding-bottom:15px"),
      tags$p(withMathJax(paste0("\\(\\hspace{30px} Y^\\text{*}_i \\sim \\mathcal{N}(\\mu_i, 1) \\quad \\text{where} \\, i = 1, \\ldots, n \\)"))),
      tags$p(paste0(
        "\\( \\hspace{30px} \\mu_i = X_i \\beta, \\; \\text{and} \\; X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)")),
      tags$p("\\( \\hspace{30px} Y^\\text{*}_i \\perp \\!\\!\\! \\perp Y^\\text{*}_j \\quad \\forall \\: i \\neq j \\)"),
      tags$p(paste0("\\( \\hspace{30px}  y_i= \\begin{cases}
    1 &\\text{if}& y^\\text{*}_i < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq y^\\text{*}_i < \\tau_1 \\\\
    3 &\\text{if}& \\tau_1 \\leq y^\\text{*}_i  \\\\
    \\end{cases} \\)")),
      tags$p(paste0("\\( \\hspace{30px} \\tau_0 = 0,\\, \\tau_0 < \\tau_1, \\, \\tau_1 = \\exp(\\gamma). \\)")),
    )
    
    
  } else if (type == "Model") {
    
    xStrs <- paste(lapply(1:nXValsAssumed, function(i){
      tmpXStr <- if(nXValsAssumed > 1){paste0("X_{i,",i,"}")} else {"X_i"}
      paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")
    
    div(tags$p(tags$b("Statistical Model ")),
        tags$p(withMathJax(paste0("\\( \\hspace{30px} Y^\\text{*}_i \\sim \\mathcal{N}(\\mu_i, 1)  \\)"))),
        tags$p(paste0(
          "\\( \\hspace{30px}\\mu_i =  X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)")),
        tags$p("\\( \\hspace{30px} Y^\\text{*}_i \\perp \\!\\!\\! \\perp Y^\\text{*}_j \\quad \\forall \\: i \\neq j \\)"),
        tags$p(paste0("\\( \\hspace{30px}  y_i= \\begin{cases}
    1 &\\text{if}& y^\\text{*}_i < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq y^\\text{*}_i < \\tau_1 \\\\
    3 &\\text{if}& \\tau_1 \\leq y^\\text{*}_i  \\\\
    \\end{cases} \\)")),
    )
    
  } else if (type == "Likelihood"){
    
    div(tags$p(tags$b(withMathJax("Likelihood for data \\(\\small y = (y_1, \\dots,y_n)\\) :"))),
        tags$p(paste0(" \\(\\hspace{30px} {\\small L(\\beta, \\gamma|y, X)= k(y) \\cdot \\prod_{i = 1}^{n} [\\text{Pr}(Y_i = j)]} \\)")),
        tags$p(tags$small("\\( \\hspace{30px} \\) where \\( k(y) \\) is an unknown function of the data: see", tags$a(href = "https://projects.iq.harvard.edu/2k1-in-silico/notation", target = "_blank", "docs"))),
        tags$p(tags$b("Log Likelihood:")),
        tags$p(paste0("\\(\\hspace{30px} {\\small \\ln[ L(\\beta, \\gamma|y, X)] \\, \\dot{=}\\, \\ln[F_{stn}(\\exp(\\gamma_j)|x_i\\beta) -  F_{stn}(\\exp(\\gamma_{j-1})|x_i\\beta)] } \\)")))
    
    
  } else if(type == "Estimation Uncertainty"){
    
    div(
      tags$p(tags$b("Estimation Uncertainty")),
      tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{\\beta} \\sim \\mathcal{N}(\\hat{\\beta}, \\hat{V}(\\hat{\\beta})) \\)")))
    )
    
  } else if(type == "Fundamental Uncertainty"){
    
    if(!is.numeric(xValsSim[[1]])) {return(div())} else{
      tmpXStr <- if(nXValsAssumed > 1){paste0("X_{c,",i,"}")} else {"X_c"}
      xStrs <- paste(lapply(1:length(xValsSim), function(i){
        paste0(" + \\tilde{\\beta_",i,"}",tmpXStr)}), collapse = "")}
    
    
    prefaceStr <- " \\tilde{\\mu_c} = X_c \\tilde{\\beta} = \\tilde{\\beta_0} "
    
    
    ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
               tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{Y}^\\text{*}_c \\sim \\mathcal{N}(\\tilde{\\mu_c}, 1)  \\)"))),
               tags$p(paste0("\\(  \\hspace{30px} \\",prefaceStr,xStrs, "\\)")),
               tags$p(paste0("\\( \\hspace{30px}  \\tilde{y}_c= \\begin{cases}
    1 &\\text{if}& \\tilde{y}^\\text{*}_c < \\tau_0 \\\\
    2 &\\text{if}& \\tau_0 \\leq \\tilde{y}^\\text{*}_c < \\tilde{\\tau_1} \\\\
    3 &\\text{if}& \\tilde{\\tau_1} \\leq \\tilde{y}^\\text{*}_c  \\\\
    \\end{cases} \\)")),
    )}
}
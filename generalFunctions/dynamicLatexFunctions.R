
##############################################
# distribution Tex
##############################################

distrLatexFunction <- function(
  type, 
  modelName,
  pdfTex, 
  pdfAddendum = 0, 
  modelDistTex, 
  modelParamTex, 
  likelihoodTex, 
  logLikelihoodTex,
  xValsSim = c(),
  paramValsPDF = c(),
  paramTex = "",
  smallLik = 0,
  smallLL = 0,
  nObs = 0){
  
  smallLikTex <- if(smallLik==1){"\\small "} else if(smallLik==2){"\\scriptsize "} else {""}
  smallLLTex <- if(smallLL){"\\small "} else if(smallLL==2){"\\scriptsize "} else {""}
  
  if(type == "Distr"){
    
    if(pdfAddendum ==1) {
      
      div(
        tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
        tags$p(paste0("\\( \\hspace{30px} \\text{where} \\quad ",modelParamTex, "\\)"))
      )
    } else if (pdfAddendum==2){
      
      
      xStrs <- paste(lapply(1:(length(paramValsPDF)-1), function(i){
        paste0(" + \\beta_",i,"X_",i)}), collapse = "")
      
      numStrs <- paste(lapply(1:(length(paramValsPDF)-1), function(i){
        paste0("+(\\color{blue}{ ", round(paramValsPDF[i+1],1), "})","(X_ ",i, ")")}), collapse = "")
      
      numStrs <- paste0("\\color{blue}{ ", round(paramValsPDF[1],1), "}", numStrs)
      
      
      div(
        tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
        tags$p(paste0("\\( \\hspace{30px} \\text{where} \\quad ",modelParamTex, "\\)")),
        tags$p(paste0("\\( \\hspace{30px} \\text{and} \\quad X_i\\beta = \\beta_0", xStrs,"\\)")),
        tags$p(paste0("\\( \\hspace{30px} = ",numStrs,"\\)")),
        tags$small("\\( \\hspace{30px} \\) with \\(i\\) from \\(1\\) to \\(",nObs,"\\) and \\(X\\) fixed: see", tags$a("Notation", onclick="customHref('Notation')")))
      
    } else {div(tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))))}
  } else if(type == "Model"){
    
    div(tags$p(tags$b("Statistical Model: ")),
        tags$p(withMathJax(paste0("\\( \\hspace{30px} Y_i \\sim ", modelDistTex,"\\)"))),
        tags$p(paste0("\\( \\hspace{30px}", modelParamTex,"\\)")),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))
    
  } else if(type == "Likelihood"){
    
    div(tags$p(tags$b(withMathJax("Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :"))),
        tags$p(paste0(" \\(\\hspace{30px}{",smallLikTex,likelihoodTex,"}\\)")),
        tags$p(tags$b("Log Likelihood:")),
        tags$p(paste0("\\(\\hspace{30px}{", smallLLTex, logLikelihoodTex," } \\)")))
    
  } else if(type == "Estimation Uncertainty"){
    
    div(
      tags$p(tags$b("Estimation Uncertainty:")),
      tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{",paramTex,"} \\sim \\mathcal{N}(\\hat{",paramTex,"}, \\hat{V}(\\hat{",paramTex,"})) \\)")))
    )
    
  } else if(type == "Fundamental Uncertainty"){
    
    
    modelTilde <- gsub(paste0("\\",paramTex), paste0(" \\\\tilde{\\",paramTex,"}"), modelDistTex)
    modelTildec <- gsub("_i", "_c", modelTilde)
    
    
    modelParamTilde <- gsub(paste0("\\",paramTex), paste0(" \\\\tilde{\\",paramTex,"}"), modelParamTex)
    modelParamTildec <- gsub("_i", "_c", modelParamTilde)
    
    
    
    if(pdfAddendum ==1) {
      
      prefaceStr <- " X_c \\tilde{\\beta} = \\tilde{\\beta_0} "
      
      
      ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
                 tags$p(paste0("\\( \\, \\hspace{30px}  \\tilde{y}_c  \\sim",modelTildec," \\)")),
                 tags$p(withMathJax(paste0(
                   "\\(  \\hspace{30px} \\,",modelParamTildec, "\\)")))
      )
      
      
    } else if(pdfAddendum ==2) {
      
      xStrs <- paste(lapply(1:length(xValsSim), function(i){
        paste0(" + \\tilde{\\beta_",i,"} X_",i)}), collapse = "")
      
      
      numStrs <- paste(lapply(1:(length(xValsSim)), function(i){
        paste0(" + \\tilde{\\beta_",i,"}(\\color{red}{ ", round(xValsSim[[i]],1), "})")}), collapse = "")
      prefaceStr <- " X_c \\tilde{\\beta} = \\tilde{\\beta_0} "
      
      
      ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
                 tags$p(paste0("\\( \\, \\hspace{30px}  \\tilde{y}_c  \\sim",modelTildec," \\)")),
                 tags$p(withMathJax(paste0("\\(  \\hspace{30px} \\,", modelParamTildec, "\\)"))),
                 tags$p(paste0("\\(  \\hspace{30px} \\",prefaceStr,xStrs, "\\)")),
                 tags$p(paste0("\\( \\hspace{30px} = \\tilde{\\beta_0} + ", numStrs,"\\)"))
      )
      
      
    } else {
      
      ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
                 tags$p(paste0("\\( \\, \\hspace{30px}  \\tilde{y}_c  \\sim",modelTildec," \\)")),
                 tags$p(withMathJax(paste0(
                   "\\(  \\hspace{30px} \\, \\tilde{",paramTex,"}_c =\\tilde{",paramTex,"}", "\\)")))
                 
      )
      
    }
    
    ret
    
    
  } else stop("Unknown Markdown!")
  
  
  
}


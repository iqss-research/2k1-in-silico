
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
  nXValsPDF = 0,
  nXValsAssumed = 0,
  xValsSim = c(),
  paramValsPDF = c(),
  nParamLL = 0,
  paramTex = "",
  metaParamTex = "",
  smallLik = 0,
  smallLL = 0,
  nObs = 0){
  
  smallLikTex <- if(smallLik==1){"\\small "} else if(smallLik==2){"\\scriptsize "} else {""}
  smallLLTex <- if(smallLL){"\\small "} else if(smallLL==2){"\\scriptsize "} else {""}
  
  if(type == "Distr"){
    
    if(pdfAddendum ==1) {
      
      div(
        tags$p(tags$b("Probability Model"), style = "padding-bottom:15px"),
        tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
        tags$p(withMathJax(paste0("\\( \\hspace{30px} \\text{where} \\, i = 1, \\ldots, n, \\)"))),
        tags$p(paste0("\\( \\hspace{30px}",modelParamTex, "\\)"))
      )
    } else if (pdfAddendum==2){
      
      
      xStrs <- paste(lapply(1:(length(paramValsPDF)-1), function(i){
        
        tmpXStr <- if(nXValsPDF > 1){paste0("X_{i,",i,"}")} else {"X_i"}
        paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")
      
      div(
        tags$p(tags$b("Probability Model"), style = "padding-bottom:15px"),
        tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
        tags$p(withMathJax(paste0("\\( \\hspace{30px} \\text{where} \\, i = 1, \\ldots, n, \\)"))),
        tags$p(paste0("\\( \\hspace{30px}",modelParamTex, "\\)")),
        tags$p(paste0(
          "\\( \\hspace{30px} \\text{and} \\quad X_i\\beta = \\color{blue}{\\beta_0}",
          xStrs,"\\)"))
      )
      
    } else if (pdfAddendum==3){
      
      
      xStrs <- paste(lapply(1:(length(paramValsPDF)-2), function(i){
        tmpXStr <- if(nXValsPDF > 1){paste0("X_{i,",i,"}")} else {"X_i"}
        paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")
      
      div(
        tags$p(tags$b("Probability Model"), style = "padding-bottom:15px"),
        tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
        tags$p(withMathJax(paste0("\\( \\hspace{30px} \\text{where} \\, i = 1, \\ldots, n, \\)"))),
        tags$p(paste0("\\( \\hspace{30px}",modelParamTex, "\\)")),
        tags$p(paste0(
          "\\( \\hspace{30px} \\text{and} \\quad X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)"))
      )
      
    } else {div(tags$p(tags$b("Probability Model"), style = "padding-bottom:15px"),
                 tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
                 tags$p(withMathJax(paste0("\\( \\hspace{30px} \\text{where} \\, i = 1, \\ldots, n, \\)"))),
                 )}
  } else if(type == "Model"){

    if(pdfAddendum > 1){
      
      if (pdfAddendum==2){
        xStrs <- paste(lapply(1:(nParamLL-1), function(i){
          tmpXStr <- if(nXValsAssumed > 1){paste0("X_{i,",i,"}")} else {"X_i"}
          paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")
      } else if (pdfAddendum>2){
        tmpXStr <- if(nXValsAssumed > 1){paste0("X_{i,",i,"}")} else {"X_i"}
        xStrs <- paste(lapply(1:(nParamLL-2), function(i){
          paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")
      }
      
      div(tags$p(tags$b("Statistical Model ")),
          tags$p(withMathJax(paste0("\\( \\hspace{30px} Y_i \\sim ", modelDistTex,"\\)"))),
          tags$p(paste0("\\( \\hspace{30px}", modelParamTex,"\\)")),
          tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"),
          tags$p(paste0(
            "\\( \\hspace{30px} \\text{and} \\quad X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)")))
    
    } else {
      div(tags$p(tags$b("Statistical Model ")),
          tags$p(withMathJax(paste0("\\( \\hspace{30px} Y_i \\sim ", modelDistTex,"\\)"))),
          tags$p(paste0("\\( \\hspace{30px}", modelParamTex,"\\)")),
          tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))}

  } else if(type == "Likelihood"){
    
    div(tags$p(tags$b(withMathJax("Likelihood for data \\(\\small y = (y_1, \\dots,y_n)\\) :"))),
        tags$p(paste0(" \\(\\hspace{30px}{",smallLikTex,likelihoodTex,"}\\)")),
        tags$p(tags$small("\\( \\hspace{30px} \\) where \\( k(y) \\) is an unknown function of the data: see", tags$a(href = "https://projects.iq.harvard.edu/2k1-in-silico/notation", target = "_blank", "docs"))),
        tags$p(tags$b("Log Likelihood:")),
        tags$p(paste0("\\(\\hspace{30px}{", smallLLTex, logLikelihoodTex," } \\)")))
    
  } else if(type == "Estimation Uncertainty"){
    
    div(
      tags$p(tags$b("Estimation Uncertainty")),
      tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{",paramTex,"} \\sim \\mathcal{N}(\\hat{",paramTex,"}, \\hat{V}(\\hat{",paramTex,"})) \\)")))
    )
    
  } else if(type == "Fundamental Uncertainty"){
    
    
    modelTilde <- gsub(paste0("\\",metaParamTex), paste0(" \\\\tilde{\\",metaParamTex,"}"), modelDistTex)
    modelTildec <- gsub("_i", "_c", modelTilde)
    
    modelParamTilde <- gsub(paste0("\\",metaParamTex), paste0(" \\\\tilde{\\",metaParamTex,"}"), modelParamTex)
    modelParamTildec <- gsub("_i", "_c", modelParamTilde)
    
    
    
    if(pdfAddendum ==1) {
      
      prefaceStr <- " X_c \\tilde{\\beta} = \\tilde{\\beta_0} "
      ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
                 tags$p(paste0("\\( \\, \\hspace{30px}  \\tilde{y}_c  \\sim",modelTildec," \\)")),
                 tags$p(withMathJax(paste0(
                   "\\(  \\hspace{30px} \\,",modelParamTildec, "\\)")))
      )
      
    } else if(pdfAddendum ==2) {
      
      if(!is.numeric(xValsSim[[1]])) {return(div())} else{
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
        )}
      
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
    
    
    sciNotTex <- function(a){
      tmp <- sprintf("%.1e", a)
      exp <- str_sub(tmp, -3, -1)
      base <- str_sub(tmp,1,3)
      paste0("{ \\small",base,"\\text{e}^{",exp," }}")}
    
    roundOrShrink <- function(a){
      if(abs(round(a,2) - 0) > 1e-5 || a == 0){return(round(a,2))} else{sciNotTex(a)}}
    
    if(any(!is.null(matrixData))){
      printStr <- paste0(header, startTex)
      rowList <- as.list(data.frame(matrixData %>%  as.matrix())) # relies on symmetry of vcov matrix
      for(r in rowList){
        tmp <- lapply(r, function(s){roundOrShrink(s)}) %>%  unlist()
        printStr <- paste0(printStr,paste(tmp, collapse = "&"),"\\\\")
        
      }
      return(withMathJax(paste0(printStr, endTex)))
    } else {return("")}},
    error = function(e){return("No values found. Check that your hessian is nonsingular.")}
    
  )
}

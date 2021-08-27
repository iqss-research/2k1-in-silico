
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
  xValsPDF = c(),
  xValsSim = c(),
  paramTex = "",
  smallLik = 0,
  smallLL = 0){
  
  smallLikTex <- if(smallLik==1){"\\small "} else if(smallLik==2){"\\scriptsize "} else {""}
  smallLLTex <- if(smallLL){"\\small "} else if(smallLL==2){"\\scriptsize "} else {""}
  
  if(type == "Distr"){
    
    if(pdfAddendum ==1) {
      
      div(
        tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
        tags$p(paste0("\\( \\hspace{30px} \\text{where} \\quad ",modelParamTex, "\\)"))
      )
    } else if (pdfAddendum==2){
      
      if(length(xValsPDF) > 0){
        
        allStrs <- paste(lapply(1:length(xValsPDF), function(i){
          paste0(" + \\beta_",i,"(\\color{red}{ ", round(xValsPDF[i],1), "})")}), collapse = "")
      } else {allStrs <- ""}
      
      div(
        tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
        tags$p(paste0("\\( \\hspace{30px} \\text{where} \\quad ",modelParamTex, "\\)")),
        tags$p(paste0("\\( \\hspace{30px} \\text{and} \\quad X_i\\beta = \\beta_0", allStrs,"\\)")),
        tags$small("\\( \\hspace{30px} \\) with X fixed: see", tags$a("Notation", onclick="customHref('Notation')")))
    } else {div(tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))))}
  } else if(type == "Model"){
    
    div(tags$p(withMathJax("Statistical Model: ")),
        tags$p(paste0("\\( \\hspace{30px} Y_i \\sim ", modelDistTex,"\\)")),
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
      tags$p(withMathJax(paste0("\\( \\hspace{30px} \\tilde{",paramTex,"} \\sim \\mathcal{N}(\\hat{",paramTex,"}, \\hat{V}(\\hat{",paramTex,"}) \\)")))
    )
    
  } else if(type == "Fundamental Uncertainty"){
    if(any(!is.null(xValsSim))){
      allStrs <- paste(lapply(1:length(xValsSim), function(i){
        paste0(" + \\beta_",i,"(\\color{red}{ ", round(xValsSim[[i]],1), "})")}), collapse = "")
      prefaceStr <- " X_c \\tilde{\\beta} = \\beta_0 "
    } else{
      allStrs <- ""
      prefaceStr <- paste0("\\tilde{",paramTex,"}")
    }
    
    modelTilde <- gsub(paste0("\\",paramTex), paste0(" \\\\tilde{\\",paramTex,"}"), modelDistTex)
    modelTildec <- gsub("X_i", "X_c", modelTilde)
    
    div(tags$p(tags$b("Fundamental Uncertainty: ")),
        tags$p(withMathJax(paste0("\\(  \\hspace{30px} \\, \\tilde{",paramTex,"}_c =",prefaceStr, allStrs, "\\)"))),
        tags$p(paste0("\\( \\, \\hspace{30px}  \\tilde{y}_c  \\sim",modelTildec," \\)"))
    )
    
  } else stop("Unknown Markdown!")
  
  
  
}


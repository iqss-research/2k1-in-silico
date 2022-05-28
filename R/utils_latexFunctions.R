
############################################################
# general Tex
############################################################

# TODO: OK we can split this up a bit

distrLatexFunction <- function(
    type,
    modelName,
    pdfTex,
    pdfAddendum = 0,
    modelDistTex,
    dgpParamTex = NA,
    modelParamTex,
    likelihoodTex,
    logLikelihoodTex,
    nXValsPDF = 0,
    nXValsAssumed = 0,
    xValsSim = c(),
    paramTex = "",
    intrParamTex = "",
    smallLik = 0,
    smallLL = 0){

  smallLikTex <- if(smallLik==1){"\\small "} else if(smallLik==2){"\\scriptsize "} else {""}
  smallLLTex <- if(smallLL){"\\small "} else if(smallLL==2){"\\scriptsize "} else {""}
  if(is.na(dgpParamTex)){dgpParamTex <- modelParamTex}


  if(type == "Distr"){

    probModelDiv <- div(
      tags$p(tags$b(
        id = "probModelHeader",
        "Probability Model"),
        style = "padding-bottom:15px;")
    )

    if(pdfAddendum ==1) {

      div(
        probModelDiv,
        tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)"))),
        tags$p(withMathJax(paste0("\\( \\hspace{30px} \\text{where} \\, i = 1, \\ldots, n \\)"))),
        tags$p(paste0("\\( \\hspace{30px}",dgpParamTex, "\\)")),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"),
      )
    } else if (pdfAddendum==2){


      xStrs <- paste(lapply(1:(nXValsPDF), function(i){

        tmpXStr <- if(nXValsPDF > 1){paste0("X_{i,",i,"}")} else {"X_i"}
        paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")

      div(
        probModelDiv,
        div(id = "pdfTex", tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)")))),
        div(id = "paramTexTut", tags$p(paste0("\\( \\hspace{30px}",dgpParamTex, "\\)")),
            tags$p(paste0(
              "\\( \\hspace{30px} \\text{and} \\quad X_i\\beta = \\color{blue}{\\beta_0}",
              xStrs,"\\)")),
            tags$p(withMathJax(paste0("\\( \\hspace{30px} \\text{where} \\, i = 1, \\ldots, n \\)")))),
        div(id = "indepTex", tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)")),

      )

    } else {div(
      probModelDiv,
      fluidRow(id = "pdfTex", tags$p(withMathJax(paste0("\\( \\hspace{30px}",pdfTex,"\\)")))),
      tags$p(withMathJax(paste0("\\( \\hspace{30px} \\text{where} \\, i = 1, \\ldots, n \\)"))),
      tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"),
    )}
  } else if(type == "Model"){

    if(pdfAddendum > 1){

      xStrs <- paste(lapply(1:(nXValsAssumed), function(i){
        tmpXStr <- if(nXValsAssumed > 1){paste0("X_{i,",i,"}")} else {"X_i"}
        paste0(" + \\color{blue}{\\beta_",i,"}",tmpXStr)}), collapse = "")

      div(tags$p(tags$b("Statistical Model ")),
          tags$p(withMathJax(paste0("\\( \\hspace{30px} Y_i \\sim ", modelDistTex,"\\)"))),
          tags$p(paste0("\\( \\hspace{30px}", modelParamTex,"\\)")),
          tags$p(paste0(
            "\\( \\hspace{30px} \\text{and} \\quad X_i\\beta = \\color{blue}{\\beta_0}", xStrs,"\\)")),
          tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"),
      )
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

    ### TODO: make this a bit cleaner
    modelTilde <- gsub(paste0("\\",paramTex), paste0(" \\\\tilde{\\",paramTex,"}"), modelDistTex)
    if(paramTex != intrParamTex){
      modelTilde <- gsub(paste0("\\",intrParamTex), paste0(" \\\\tilde{\\",intrParamTex,"}"), modelTilde)
    }
    modelTilde <- gsub("\\\\sigma", " \\\\tilde{\\\\sigma}", modelTilde)
    modelTildec <- gsub("_i", "_c", modelTilde)

    modelParamTilde <- gsub(paste0("\\",paramTex), paste0(" \\\\tilde{\\",paramTex,"}"), modelParamTex)
    modelParamTilde <- gsub(paste0("\\",intrParamTex), paste0(" \\\\tilde{\\",intrParamTex,"}"), modelParamTilde)
    modelParamTilde <- gsub("\\\\sigma", " \\\\tilde{\\\\sigma}", modelParamTilde)
    modelParamTilde <- gsub("\\\\gamma", " \\\\tilde{\\\\gamma}", modelParamTilde)
    modelParamTildec <- gsub("_i", "_c", modelParamTilde)

    if(pdfAddendum ==1) {

      prefaceStr <- " X_c \\tilde{\\beta} = \\tilde{\\beta_0} "
      ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
                 tags$p(withMathJax(paste0("\\(  \\hspace{30px} \\,",modelParamTildec, "\\)"))),
                 tags$p(paste0("\\( \\, \\hspace{30px}  \\tilde{y}_c  \\sim",modelTildec," \\)")),
      )

    } else if(pdfAddendum ==2) {
      if(!is.numeric(xValsSim[[1]])) {return(div())} else{
        if(length(xValsSim) == 1) {
          xStrs <-  paste0(" + \\tilde{\\beta_1} X_c")

        } else {
          xStrs <- paste(lapply(1:length(xValsSim), function(i){
            paste0(" + \\tilde{\\beta_",i,"} X_{c,",i, "}")}), collapse = "")

        }

        numStrs <- paste(lapply(1:(length(xValsSim)), function(i){
          paste0(" + \\tilde{\\beta_",i,"}(\\color{red}{ ", round(xValsSim[[i]],1), "})")}), collapse = "")
        prefaceStr <- " X_c \\tilde{\\beta} = \\tilde{\\beta_0} "


        ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
                   tags$p(withMathJax(paste0("\\(  \\hspace{30px} \\,", modelParamTildec, "\\)"))),
                   tags$p(paste0("\\(  \\hspace{30px} \\",prefaceStr,xStrs, "\\)")),
                   tags$p(paste0("\\( \\hspace{30px} = \\tilde{\\beta_0}", numStrs,"\\)")),
                   tags$p(paste0("\\( \\, \\hspace{30px}  \\tilde{y}_c  \\sim",modelTildec," \\)")),
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

coeffLatex <- function(paramTex, secondaryParamTex, coeffData){

  nParams <- length(coeffData)
  if(nParams == 1){
    return(tags$p(
      paste0("\\( \\hat{",paramTex[1], "}  = ",roundOrShrink(coeffData[1]),"\\)" )))
  }

  paramTexList <- paste0(paramTex, "_", 0:(nParams-1))
  if(!is.na(
    tryCatch( eval(parse(text = secondaryParamTex)), error = function(e){ 1 }) # TODO: unhack
  )){paramTexList[length(paramTexList)] <- secondaryParamTex}
  if(length(paramTexList) != length(coeffData)){return("")}


  paramStrs <- lapply(1:length(coeffData), function(i){
    tmp <- if(i ==1){"\\;"} else {", \\;"}
    paste0(tmp,"\\hat{",paramTexList[i],"}" )
  })

  numStrs <- lapply(1:length(coeffData), function(i){
    tmp <- if(i ==1){""} else {", \\;"}
    paste0(tmp,roundOrShrink(coeffData[i]) )
  })

  div(
    tags$p(withMathJax(paste0("\\( \\begin{aligned}
                  \\hat{\\theta} =& [\\; ", paste(paramStrs, collapse = ""), "\\;] \\\\",
                              "=& [",paste(numStrs, collapse = ""), "\\;]\\\\",
                              "\\end{aligned}\\)"))),
  )
}


vCovLatex  <- function(paramTexList, matrixData){
  tryCatch({
    if(length(matrixData) == 1){
      return(tags$p(paste0(
        "\\( \\hat{V}(\\hat{",paramTexList[1], "}) = ",roundOrShrink(matrixData[1]), " \\)" )))
    } else {
      if(ncol(matrixData) > 3){
        startTex <- "{\\small \\begin{bmatrix}"
        endTex <- "\\end{bmatrix} } \\)"
      } else{
        startTex <- "\\begin{bmatrix}"
        endTex <- "\\end{bmatrix} \\)"
      }
    }


    if(any(!is.null(matrixData))){
      printStr <- paste0("\\(\\hat{V}(\\hat{\\theta}) =", startTex)
      rowList <- as.list(data.frame(matrixData %>%  as.matrix())) # relies on symmetry of vcov matrix
      for(r in rowList){
        tmp <- lapply(r, function(s){roundOrShrink(s)}) %>%  unlist()
        printStr <- paste0(printStr,paste(tmp, collapse = "&"),"\\\\")

      }
      return(tags$p(withMathJax(paste0(printStr, endTex))))
    } else {return("")}},
    error = function(e){return("No values found. Check that your hessian is nonsingular.")}

  )
}


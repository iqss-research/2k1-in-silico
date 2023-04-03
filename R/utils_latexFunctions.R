
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
        tags$p(HTML(katex_html(paste0("\\hspace{5px}",pdfTex),
                               preview = FALSE,
                               output = "html"))),
        tags$p(HTML(katex_html("\\hspace{5px} \\text{where} \\, i = 1, \\ldots, n",
                    preview = FALSE,
                    output = "html"))),
        tags$p(HTML(katex_html(paste0("\\hspace{5px}",dgpParamTex),
                    preview = FALSE,
                    output = "html"))),
        tags$p(HTML(katex_html("\\hspace{5px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j",
                    preview = FALSE,
                    output = "html"))),
      )
    } else if (pdfAddendum==2){


      xStrs <- paste(lapply(1:(nXValsPDF), function(i){

        tmpXStr <- if(nXValsPDF > 1){paste0("X_{i,",i,"}")} else {"X_i"}
        paste0(" + {\\color{blue}{\\beta_",i,"}}",tmpXStr)}), collapse = "")

      div(
        probModelDiv,
        div(id = "pdfTex", tags$p(HTML(katex_html(paste0("\\hspace{5px}",pdfTex),
                                                  preview = FALSE,
                                                  output = "html")))),
        div(id = "paramTexTut", tags$p(HTML(katex_html(paste0("\\hspace{5px}",dgpParamTex),
                                                       preview = FALSE,
                                                       output = "html"))),
            tags$p(HTML(katex_html(paste0(
              "\\hspace{5px} \\text{and} \\quad X_i\\beta = {\\color{blue}{\\beta_0}}",
              xStrs),
              preview = FALSE,
              output = "html"))),
            tags$p(HTML(katex_html("\\hspace{5px} \\text{where} \\, i = 1, \\ldots, n",
                        preview = FALSE,
                        output = "html")))),
        div(id = "indepTex", tags$p(HTML(katex_html("\\hspace{5px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j",
                                         preview = FALSE,
                                         output = "html")))),

      )

    } else {div(
      probModelDiv,
      fluidRow(id = "pdfTex", tags$p(HTML(katex_html(paste0("\\hspace{5px}",pdfTex),
                                                     preview = FALSE,
                                                     output = "html")))),
      tags$p(HTML(katex_html("\\hspace{5px} \\text{where} \\, i = 1, \\ldots, n",
                  preview = FALSE,
                  output = "html"))),
      tags$p(HTML(katex_html("\\hspace{5px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j",
                  preview = FALSE,
                  output = "html"))),
    )}
  } else if(type == "Model"){

    if(pdfAddendum > 1){

      xStrs <- paste(lapply(1:(nXValsAssumed), function(i){
        tmpXStr <- if(nXValsAssumed > 1){paste0("X_{i,",i,"}")} else {"X_i"}
        paste0(" + {\\color{blue}{\\beta_",i,"}}",tmpXStr)}), collapse = "")

      div(tags$p(tags$b("Statistical Model ")),
          tags$p(HTML(katex_html(paste0("\\hspace{5px} Y_i \\sim ", modelDistTex),
                                 preview = FALSE,
                                 output = "html"))),
          tags$p(HTML(katex_html(paste0("\\hspace{5px}", modelParamTex),
                                 preview = FALSE,
                                 output = "html"))),
          tags$p(HTML(katex_html(paste0(
            "\\hspace{5px} \\text{and} \\quad X_i\\beta = {\\color{blue}{\\beta_0}}", xStrs),
            preview = FALSE,
            output = "html"))),
          tags$p(HTML(katex_html("\\hspace{5px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j",
                      preview = FALSE,
                      output = "html"))),
      )
    } else {
      div(tags$p(tags$b("Statistical Model ")),
          tags$p(HTML(katex_html(paste0("\\hspace{5px} Y_i \\sim ", modelDistTex),
                                 preview = FALSE,
                                 output = "html"))),
          tags$p(HTML(katex_html(paste0("\\hspace{5px}", modelParamTex),
                                 preview = FALSE,
                                 output = "html"))),
          tags$p(HTML(katex_html("\\hspace{5px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j",
                      preview = FALSE,
                      output = "html"))))}

  } else if(type == "Likelihood"){

    div(tags$p(tags$b(HTML(katex_html("\\text{Likelihood for data } \\small y = (y_1, \\dots,y_n) :",
                           preview = FALSE,
                           output = "html")))),
        tags$p(HTML(katex_html(paste0("\\hspace{5px}{",smallLikTex,likelihoodTex,"}"),
                               preview = FALSE,
                               output = "html"))),
        tags$p(tags$small(HTML(katex_html("\\hspace{5px} \\text{ where } k(y) \\text{is an unknown function of the data.}",
                               preview = FALSE,
                               output = "html")))),
        tags$p(tags$b("Log Likelihood:")),
        tags$p(HTML(katex_html(paste0("\\hspace{5px}{", smallLLTex, logLikelihoodTex," }"),
                               preview = FALSE,
                               output = "html",
                               strict = FALSE))))

  } else if(type == "Estimation Uncertainty"){

    div(
      tags$p(tags$b("Estimation Uncertainty")),
      tags$p(HTML(katex_html(paste0("\\hspace{5px} \\tilde{",paramTex,"} \\sim \\mathcal{N}(\\hat{",paramTex,"}, \\hat{V}(\\hat{",paramTex,"}))"),
                             preview = FALSE,
                             output = "html")))
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
                 tags$p(HTML(katex_html(paste0("\\hspace{5px} \\,",modelParamTildec),
                                        preview = FALSE,
                                        output = "html"))),
                 tags$p(HTML(katex_html(paste0("\\, \\hspace{5px}  \\tilde{y}_c  \\sim",modelTildec),
                                        preview = FALSE,
                                        output = "html"))),
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
          paste0(" + \\tilde{\\beta_",i,"}({\\color{red}{ ", round(xValsSim[[i]],1), "}})")}), collapse = "")
        prefaceStr <- " X_c \\tilde{\\beta} = \\tilde{\\beta_0} "


        ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
                   tags$p(HTML(katex_html(paste0("\\hspace{5px} \\,", modelParamTildec),
                                          preview = FALSE,
                                          output = "html"))),
                   tags$p(HTML(katex_html(paste0("\\hspace{5px} \\",prefaceStr,xStrs),
                                          preview = FALSE,
                                          output = "html"))),
                   tags$p(HTML(katex_html(paste0("\\hspace{5px} = \\tilde{\\beta_0}", numStrs),
                                          preview = FALSE,
                                          output = "html"))),
                   tags$p(HTML(katex_html(paste0("\\, \\hspace{5px}  \\tilde{y}_c  \\sim",modelTildec),
                                          preview = FALSE,
                                          output = "html"))),
        )}

    } else {

      ret <- div(tags$p(tags$b("Fundamental Uncertainty")),
                 tags$p(HTML(katex_html(paste0("\\, \\hspace{5px}  \\tilde{y}  \\sim",modelTilde),
                                        preview = FALSE,
                                        output = "html"))),

                  tags$p(ifelse(intrParamTex != paramTex,
                                HTML(katex_html(
                                  paste0("\\hspace{5px} \\, \\tilde{",
                                         intrParamTex,"} =\\tilde{",
                                         paramTex,"}"),
                                  preview = FALSE,
                                  output = "html")),
                                  "")),
      )
    }
    ret

  } else stop("Unknown Markdown!")

}

############################################################
# MLE LaTeX
############################################################

coeffLatex <- function(paramTex, secondaryParamTex, coeffData){

  nParams <- length(coeffData)
  if(nParams == 1){
    return(tags$p(
      HTML(katex_html(paste0("\\hat{",paramTex[1], "}  = ",
                            roundOrShrink(coeffData[1])),
                      preview = FALSE,
                      output = "html"))))
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
    tags$p(HTML(katex_html(paste0("\\begin{aligned}
                  \\hat{\\theta} =& [\\; ", paste(paramStrs, collapse = ""), "\\;] \\\\",
                              "=& [",paste(numStrs, collapse = ""), "\\;]\\\\",
                              "\\end{aligned}"),
                           preview = FALSE,
                           output = "html"))),
  )
}


vCovLatex  <- function(paramTexList, matrixData){
  tryCatch({
    if(length(matrixData) == 1){
      return(tags$p(HTML(katex_html(paste0(
        "\\hat{V}(\\hat{",paramTexList[1], "}) = ",roundOrShrink(matrixData[1])),
        preview = FALSE,
        output = "html"))))
    } else {
      if(ncol(matrixData) > 3){
        startTex <- "{\\small \\begin{bmatrix}"
        endTex <- "\\end{bmatrix} }"
      } else{
        startTex <- "\\begin{bmatrix}"
        endTex <- "\\end{bmatrix}"
      }
    }


    if(any(!is.null(matrixData))){
      printStr <- paste0("\\hat{V}(\\hat{\\theta}) =", startTex)
      rowList <- as.list(data.frame(matrixData %>%as.matrix())) # relies on symmetry of vcov matrix
      for(r in rowList){
        tmp <- lapply(r, function(s){roundOrShrink(s)}) %>%  unlist()
        printStr <- paste0(printStr,paste(tmp, collapse = "&"),"\\\\")

      }
      return(tags$p(HTML(katex_html(paste0(printStr, endTex),
                                    preview = FALSE,
                                    output = "html"))))
    } else {return("")}},
    error = function(e){return("No values found. Check that your hessian is nonsingular.")}

  )
}


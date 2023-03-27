# Util functions across UI tabs

############################################################

marginalSelectInput <- function(ns,
                                choicesInput = c(),
                                fixedValues = NULL,
                                currentChoice = NULL,
                                inputID = "marginalSelectedLL",
                                hidden = F,
                                session = session){
  if(length(choicesInput)==0){choicesInput <- c(rep(1, length(choicesInput)))}
  if(is.null(fixedValues)||(length(fixedValues) != length(choicesInput))){fixedValues <- c(rep(1, length(choicesInput)))}
  if(is.null(currentChoice)){currentChoice <- choicesInput[1]}

  if(length(choicesInput)==1){return(
    div(
      choicesInput[1]
    )

  )}

  titleStr <- if(inputID == "marginalSelectedLL"){ "Choose a Profile Likelihood"
  } else { "Choose a Covariate"}

  output <- tryCatch({fluidRow(
    column(2,div(
      selectInput(
        inputId = ns(inputID),
        label = NULL,
        choices = choicesInput,
        selected = currentChoice,
        width = "100px" ),
      style = "float:left;")),
  )}, error = function(e){
    div(selectInput(
        inputId = ns("marginalSelectedLL"),
        label = NULL,
        choices = c("Beta0"),
        selected = "Beta0",
        width = "100px"),
      style = "display:none;" )})

  if(hidden){return(div(output,
                        style = "display:none;"))}
  else{return(output)}

}


############################################################
# Div with choices of X
############################################################

xChoiceDivFun <- function(ns=NULL, choices = NULL,assumed = F, hidden = F, plus = F, minus = F){
  if(is.null(choices)){choices <- c("Uniform B", "Normal A", "Bernoulli C")}

  nChoices <- length(choices)
  inputIDStr <- if(!assumed){paste0("xChoice",1:nChoices )} else{paste0("assumedXChoice",1:nChoices )}

  output <- div(
    column(12,
           lapply(1:nChoices, function(i){

             fluidRow(
               tags$p(HTML(katex_html(paste0("X_",i),
                                      displayMode = TRUE,
                                      preview = FALSE,
                                      output = "html")),
                      style = "float:left; padding-right:10px;"),
               div(id = paste0(inputIDStr[i],"Div"),
                   selectInput(
                     inputId = ns(inputIDStr[i]),
                     label = NULL,
                     choices = xGenerationChoices,
                     selected = choices[i],
                     width = "150px"),
                   style = "float:left;"),
               title = "Choose from fixed, pre-generated covariates"
             )
           }),
           if(plus & minus){
             div(
               div(actionButton(ns(paste0("addXVar", ifelse(assumed, "Assumed", ""))), label = icon("plus")),
                   style = "padding-left:15px; padding-bottom:10px; display:inline-block", title = "Add a covariate"),
               div(actionButton(ns(paste0("subtractXVar", ifelse(assumed, "Assumed", ""))), label = icon("minus")),
                   style = "padding-left:15px; padding-bottom:10px;display:inline-block", title = "Remove a covariate")
             )
           }
           else if(plus) {
             div(actionButton(ns(paste0("addXVar", ifelse(assumed, "Assumed", ""))), label = icon("plus")),
                 style = "padding-left:15px; padding-bottom:10px", title = "Add a covariate")
           } else if(minus) {
             div(actionButton(ns(paste0("subtractXVar", ifelse(assumed, "Assumed", ""))), label = icon("minus")),
                 style = "padding-left:15px; padding-bottom:10px", title = "Remove a covariate")
           } else {div()}
    )
  )

  if(hidden){return(div(output, style = "display:none;"))} else{return(output)}
}




#############################################################
# Generate X Values
#############################################################

# returns first nRow rows and nCol cols
# where nRow shd be n and nCol shd be k
# first col always 1
xValGenerator <- function(nRow, type=c("Bernoulli A")){

  nType <- length(type)
  # TODO: make extensible to more than 2 cases
  if(!any(is.null(type))){
    cbind(
      `allXConstant (1)`[1:nRow,1],
      lapply(
        1:nType,
        function(i){
          eval(parse(text = paste0("`allX",type[i],"`[1:nRow, 1]")))}) %>%
        unlist() %>%
        matrix(nRow, length(type))
    )
  } else {`allXConstant (1)`[1:nRow,1]}
}


############################################################
# Slider Maker
############################################################


# TODO refactor
manyParamSliderMaker <- function(
    ns=NULL,
    minVal=-1,
    maxVal = 1,
    startVals = c(1,-1,0),
    stepVal = .01,
    paramTex = "",
    secondParamTex = NA,
    inputName= "param",
    sigmaScale = NA){

  if(length(startVals) == 0) return(div())
  nParams <- length(startVals[!is.na(startVals)])
  paramHTML <-  paste0("&",substr(paramTex,2, 999),";")
  secondParamHTML <-  if(isnothing(secondParamTex)) {"&sigma;"} else {paste0("&",substr(secondParamTex,2, 999),";")}
  multi <- if(
    (nParams > 1) &(length(sigmaScale)>0) & (!is.na(parser(sigmaScale)))){ "fullNorm"}
  else if((nParams > 1)){
    "betas"} else {"none"}


  output <- if(multi=="betas") {

    div(

      lapply(1:nParams, function(i){
        column(12,
               div(HTML(paste0("<p style='color:#0000ff'><b>&beta;<sub>",(i-1),"</sub></b></p>")),
                   style = "float:left; padding-right:10px"),
               div(id = paste0("assumedParamSliderDiv",i),
                   sliderInput(
                     ns(paste0(inputName,i)),
                     NULL,
                     min = minVal,
                     max = maxVal,
                     value = startVals[i],
                     step = stepVal,
                     width = paramSliderWidth), style = "float:left;"),
               if(i == 1 & inputName=="param") helperMaker("Parameters"),
        )
      })

    )
  } else if (multi == "fullNorm") {
    div(
      lapply(1:(nParams-1), function(i){
        column(12,
               div(HTML(paste0("<p style='color:#0000ff'><b>&beta;<sub>",(i-1),"</sub></b></p>")),
                   style = "float:left; padding-right:10px"),
               div(id = paste0("paramSliderDiv",i),
                   sliderInput(
                     ns(paste0(inputName,i)),
                     NULL,
                     min = minVal,
                     max = maxVal,
                     value = startVals[i],
                     step = stepVal,
                     width = paramSliderWidth), style = "float:left;"),
               if(i == 1 & inputName=="param") helperMaker("Parameters"),
        )
      }),
      column(12,
             div(HTML(if(inputName == "param") # sigma on the first page
             {paste0("<p style='color:#0000ff'><b>",secondParamHTML, "</b></p>")
             } else {
               "<p style='color:#0000ff'><b>&gamma;</b></p>"}),
             style = "float:left; padding-right:10px"),
             div(id = "paramSliderDivS",
                 sliderInput(
                   ns(paste0(inputName,nParams)),
                   NULL,
                   min = sigmaScale[1],
                   max = sigmaScale[2],
                   value = startVals[nParams],
                   step = 0.01,
                   width = paramSliderWidth),
                 style = "float:left;" ))
    )
  } else{
    div(
      column(12,
             div(HTML(paste0("<p style='color:#0000ff'><b>",paramHTML,"</b></p>")),
                 style = "float:left; padding-right:10px"),
             div(sliderInput(
               ns(paste0(inputName,"1")),
               NULL,
               min = minVal,
               max = maxVal,
               value = startVals[1],
               step = stepVal,
               width = paramSliderWidth),
               style = "float:left;")
      )
    )
  }
}




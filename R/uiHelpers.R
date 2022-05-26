############################################################
# Tooltip maker 
############################################################
helperMaker <- function(str, styleArg = ""){
  
  # An unholy hybrid of icons inspired by shinyhelper package
  # and popovers from good ol bootstrap
  # TODO: can we have this also trigger a rintrojs option for eg. 
  # the probability model which is long
  browser()
  withMathJax(div(
    popify(a(class = "helpercirc", icon(
      name = "info-circle",
      class = "shinyhelper-icon"), tabindex = 0),
      title = str,
      content = HTML(
        (filter(tutorialText,Name == str))$content),
      placement = "right", trigger = "focus",
      options =  list(container = "body")
    ),
    class = "shinyhelper-container",
    style = styleArg,
    
  ))
}

helperMakerNavbar <- function(str, styleArg = ""){
  
  withMathJax(div(
    class = "shinyhelper-container-navbar",
    tags$script(
      paste0("
      $('.shinyhelper-container-navbar').click(function(event){
    event.stopPropagation();});
             "),
    ),
    popify(
      a(
        class = "helpercirc-navbar", icon(
          name = "info-circle",
          class = "shinyhelper-icon-navbar"),
        tabindex = 0),
      title = str,
      content = HTML(
        (filter(tutorialText, Name == str))$content),
      placement = "bottom", trigger = "click",
      options =  list(container = "body")
    ),
    style = styleArg
  ))
}


introBox <- function (..., data.step, data.intro, data.hint, data.title = "", data.position = c("bottom", 
                                                                                                "auto", "top", "left", "right", "bottom", "bottom-left_aligned", 
                                                                                                "bottom-middle-aligned", "bottom-right-aligned", "auto")) 
{
  stopifnot(!((!missing(data.step) & missing(data.intro)) | 
                (missing(data.step) & !missing(data.intro))))
  data.position <- match.arg(data.position)
  data <- match.call(expand.dots = TRUE)
  n <- length(list(...)) + 1
  names(data)[-seq_len(n)] <- gsub("\\.", "-", names(data)[-seq_len(n)])
  data[[1]] <- quote(shiny::tags$div)
  eval.parent(data)
}


############################################################
# Slider Maker
############################################################

obsHeaderFun <- function(nVars){tags$p(tags$b(if(nVars == 1){"Draws of Y"} else {"Models (1 draw each)"}))}


obsSliderFun <- function(nVars){
  
  titleStr <- if(nVars == 1){"Draws of Y"} else {"Number of Models (1 draw each)"}
  
  fluidRow(
    tags$p(tags$b("Parameters"), style = "padding-top:15px;padding-left:15px;"),
    column(
      12,
      div(
        div(tags$p(tags$b("n")), style = "color:#ff0000; float:left; padding-right:10px;"),
        div(sliderInput("nObs",
                        NULL,
                        min = 1,
                        max = 200,
                        value = 50,
                        step = 1, 
                        width = paramSliderWidth),
            style = "float:left;"),
        style= "padding-left:15px;", title = titleStr
      ))
  )}



# TODO refactor
manyParamSliderMaker <- function(
    minVal=-1,
    maxVal = 1,
    startVals = c(1,-1,0),
    stepVal = .01,
    paramTex = "",
    secondParamTex = NA,
    inputName= "param",
    sigmaScale = NA){
  
  # browser()
  if(length(startVals) == 0) return(div())
  nParams <- length(startVals[!is.na(startVals)])
  paramHTML <-  paste0("&",substr(paramTex,2, 999),";")
  secondParamHTML <-  if(isnothing(secondParamTex)) {"&sigma;"} else {paste0("&",substr(secondParamTex,2, 999),";")}
  multi <- if((nParams > 1) &(!is.na(sigmaScale))){ "fullNorm"} else if((nParams > 1)){
    "betas"} else {"none"}
  # browser()
  
  output <- if(multi=="betas") {
    
    div(
      
      lapply(1:nParams, function(i){
        column(12, 
               div(HTML(paste0("<p style='color:#0000ff'><b>&beta;<sub>",(i-1),"</sub></b></p>")),
                   style = "float:left; padding-right:10px"),
               div(id = paste0("assumedParamSliderDiv",i),
                   sliderInput(
                     paste0(inputName,i),
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
                     paste0(inputName,i),
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
             div(id = "paramSliderDivS",sliderInput(
               paste0(inputName,nParams),
               NULL,
               min = sigmaScale[1],
               max = sigmaScale[2],
               value = startVals[nParams],
               step = 0.01,
               width = paramSliderWidth),style = "float:left;" ))
    )
  } else{
    div(
      column(12, 
             div(HTML(paste0("<p style='color:#0000ff'><b>",paramHTML,"</b></p>")),
                 style = "float:left; padding-right:10px"),
             div(sliderInput(
               paste0(inputName,"1"),
               NULL,
               min = minVal,
               max = maxVal,
               value = startVals[1],
               step = stepVal,
               width = paramSliderWidth), style = "float:left;")
      )
    )
  }
}

############################################################
# Tooltip Function
############################################################


# tooltipFun <- function(hover, text){
#   if(length(hover)==0) return(NULL)
#   # calculate point position INSIDE the image as percent of total dimensions
#   # from left (horizontal) and from top (vertical)
#   left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
#   top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
#   
#   # calculate distance from left and bottom side of the picture in pixels
#   left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
#   top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
#   
#   style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
#                   "left:", left_px + 2, "px; top:", top_px + 2, "px;")
#   
#   wellPanel(
#     style = style,
#     tags$p(tags$small(text))
#   )}


############################################################
# Printing number outputs
############################################################
dataHeaderFun <- function(grp){tags$p(tags$b(if(grp == "Real"){"Observed Ys"} else {"Data Generation, Y"}))}


### TODO merge these print functions
dataPrintHelper <- function(data, printLength){
  
  if(all(data == round(data))) {
    
    printStr <- paste(c(data), sep = " ")
    if(length(data) > printLength){printStr <- paste0(printStr, " ...")}
    printStr <-paste(printStr, collapse = " ")
    
  } else{
    
    if(length(data) > printLength){truncData <- data[1:printLength]}
    else{truncData <- data}
    charData <- lapply(truncData, function(s){sprintf("%0.1f",s)}) %>%  unlist()
    
    printStr <- paste(c(charData), collapse = ", ")
    printStr <- paste(printStr, sep = "")
    if(length(data) > printLength){printStr <- paste0(printStr, " ...")}
  }
  
  printStr
}

############################################################
# Div with choices of X
############################################################

xChoiceDivFun <- function(choices = NULL,assumed = F, hidden = F, plus = F, minus = F){
  if(is.null(choices)){choices <- c("Uniform B", "Normal A", "Bernoulli C")}
  
  nChoices <- length(choices)
  inputIDStr <- if(!assumed){paste0("xChoice",1:nChoices )} else{paste0("assumedXChoice",1:nChoices )}
  
  output <- div(
    column(12, 
           lapply(1:nChoices, function(i){
             
             fluidRow(
               tags$p(withMathJax(paste0("\\(X_",i,"\\)")), style = "float:left; padding-right:10px;"),
               div(id = paste0(inputIDStr[i],"Div"), selectInput(
                 inputId = inputIDStr[i],
                 label = NULL,
                 choices = xGenerationChoices,
                 selected = choices[i],
                 width = "150px"), style = "float:left;"), title = "Choose from fixed, pre-generated covariates"
             )
           }),
           if(plus & minus){
             div(
               div(actionButton(paste0("addXVar", ifelse(assumed, "Assumed", "")), label = icon("plus")),
                   style = "padding-left:15px; padding-bottom:10px; display:inline-block", title = "Add a covariate"),
               div(actionButton(paste0("subtractXVar", ifelse(assumed, "Assumed", "")), label = icon("minus")),
                   style = "padding-left:15px; padding-bottom:10px;display:inline-block", title = "Remove a covariate")
             )
           }
           else if(plus) {
             div(actionButton(paste0("addXVar", ifelse(assumed, "Assumed", "")), label = icon("plus")),
                 style = "padding-left:15px; padding-bottom:10px", title = "Add a covariate")
           } else if(minus) {
             div(actionButton(paste0("subtractXVar", ifelse(assumed, "Assumed", "")), label = icon("minus")),
                 style = "padding-left:15px; padding-bottom:10px", title = "Remove a covariate")
           } else {div()}
    )
  )
  
  if(hidden){return(div(output, style = "display:none;"))} else{return(output)}
}


############################################################
# MLE UI
############################################################

marginalSelectInput <- function(choicesInput = c(),
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
        inputId = inputID,
        label = NULL,
        choices = choicesInput, selected = currentChoice,
        width = "100px" ), style = "float:left;")),
  )}, error = function(e){
    div(selectInput(
      inputId = "marginalSelectedLL",
      label = NULL,
      choices = c("Beta0"), selected = "Beta0",
      width = "100px"), style = "display:none;" )})
  
  if(hidden){return(div(output, style = "display:none;"))} else{return(output)}
  
}





############################################################
# Sim UI
############################################################


# function making sliders for the sim pages
simMultiSliderFunction <- function(numSliders){
  
  if(numSliders == 0){""} else{
    
    column(12,
           lapply(1:numSliders, function(i){
             column(12,div(
               div(HTML(paste0("<p style='color:#ff0000'><b>X<sub>c, ",i,"s</sub></b></p>")),
                   style = "float:left; padding-right:10px"),
               div(
                 id = paste0("simSliderDiv",i),
                 sliderInput(
                   paste0("simX",i),
                   NULL,
                   min = -2,
                   max = 2,
                   value = (-i)^2*.1,
                   step = .1,
                   width = paramSliderWidth), style = "float:left;")))
           }), style = "margin-left:0px"
    )
  }
  
}





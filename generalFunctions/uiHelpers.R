############################################################
# Slider Maker
############################################################

obsHeaderFun <- function(nVars){tags$p(tags$b(if(nVars == 1){"Draws of Y"} else {"Models (1 draw each)"}))}


obsSliderFun <- function(nVars){
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
                        value = 20,
                        step = 1, 
                        width = paramSliderWidth),
            style = "float:left;"), style= "padding-left:15px;"
      ))
  )}



# TODO refactor
manyParamSliderMaker <- function(minVal=-1, maxVal = 1, startVals = c(1,-1,0), stepVal = .1, multi ="betas", paramHTML = "", inputName= "param", sigmaScale = c(0,0)){
  
  nParams <- length(startVals)
  if(multi=="betas") {
    
    div(
      
      lapply(1:nParams, function(i){
        column(12, 
               div(HTML(paste0("<p style='color:#0000ff'><b>&beta;<sub>",(i-1),"</sub></b></p>")),
                   style = "float:left; padding-right:10px"),
               div(sliderInput(
                 paste0(inputName,i),
                 NULL,
                 min = minVal,
                 max = maxVal,
                 value = startVals[i],
                 step = stepVal,
                 width = paramSliderWidth), style = "float:left;"))
      })
      
    )
  } else if (multi == "fullNorm") {
    div(
      
      lapply(1:(nParams-1), function(i){
        column(12, 
               div(HTML(paste0("<p style='color:#0000ff'><b>&beta;<sub>",(i-1),"</sub></b></p>")),
                   style = "float:left; padding-right:10px"),
               div(sliderInput(
                 paste0(inputName,i),
                 NULL,
                 min = minVal,
                 max = maxVal,
                 value = startVals[i],
                 step = stepVal,
                 width = paramSliderWidth), style = "float:left;"))
      }),
      column(12,
             div(HTML(paste0("<p style='color:#0000ff'><b>&gamma;</b></p>")),
                 style = "float:left; padding-right:10px"),
             div(sliderInput(
               paste0(inputName,nParams),
               NULL,
               min = sigmaScale[1],
               max = sigmaScale[2],
               value = startVals[nParams],
               step = 0.1,
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
               width = paramSliderWidth), style = "float:left;")),
    )
  }
}

############################################################
# Printing number outputs
############################################################


### TODO merge these print functions
decPrintHelper <- function(header, data, printLength){
  
  if(length(data) > printLength){truncData <- data[1:printLength]}
  else{truncData <- data}
  charData <- lapply(truncData, function(s){sprintf("%0.1f",s)}) %>%  unlist()
  
  printStr <- paste(c(charData), collapse = ", ")
  printStr <- paste(header, printStr, sep = "")
  if(length(data) > printLength){printStr <- paste0(printStr, " ...")}
  
  printStr
}


intPrintHelper <- function(header, data, printLength){
  
  printStr <- paste(c(header, data), sep = " ")
  if(length(data) > printLength){printStr <- paste0(printStr, " ...")}
  printStr <-paste(printStr, collapse = " ")
}



############################################################
# Div with choices of X
############################################################

# TODO: refactor. can remove printing
xChoiceDivFun <- function(
  vals = matrix(rep(NA, 60), 20,3),
  nObs = 20, 
  choices = NULL,
  assumed = F,
  hidden = F,
  xChoices = xGenerationChoices){
  
  if(is.null(choices)){choices <- c("Normal(0,1)", "Uniform(0,1)")}
  nChoices <- length(choices)
  inputIDStr <- if(!assumed){paste0("xChoice",1:nChoices )} else{paste0("assumedXChoice",1:nChoices )}
  
  
  output <- div(
    column(12, 
           lapply(1:nChoices, function(i){
             
             fluidRow(
               tags$p(withMathJax(paste0("\\(X_",i,"\\)")), style = "float:left; padding-right:10px;"),
               div(selectInput(
                 inputId = inputIDStr[i],
                 label = NULL,
                 choices = xGenerationChoices,
                 selected = choices[i],
                 width = "150px"), style = "float:left;")
             )
           })
    )
  )

if(hidden){return(div(output, style = "display:none;"))} else{return(output)}
}




############################################################
# Real Data X choices
############################################################

realXChoiceDivFun <- function(choices){
  
  
}


############################################################
# MLE UI
############################################################

marginalSelectInput <- function(choicesInput = c(),
                                fixedValues = NULL, 
                                currentChoice = NULL, 
                                inputID = "marginalSelectedLL",
                                includeBetas = T,
                                hidden = F,
                                session = session){
  
  if(is.null(choicesInput)){choicesInput <- c(rep(1, length(choicesInput)))}
  if(is.null(fixedValues)||(length(fixedValues) != length(choicesInput))){fixedValues <- c(rep(1, length(choicesInput)))}
  if(is.null(currentChoice)){currentChoice <- choicesInput[1]}
  
  unselectedInx <- (1:length(choicesInput))[-which(choicesInput == currentChoice)]
  unselected <- fixedValues[unselectedInx]
  
  output <- tryCatch({fluidRow(
    div(
      selectInput(
        inputId = inputID,
        label = NULL,
        choices = choicesInput, selected = currentChoice,
        width = "100px" ), style = "float:left;"),
    if(includeBetas){div(
      tags$p(withMathJax(
        paste0("\\( (",
               paste(
                 sapply(1:length(unselectedInx), function(p){
                   paste0(if(unselectedInx[p] ==4){"{\\sigma"} else{ paste0("\\beta_{",
                                                                            unselectedInx[p]-1)}, "} = ", unselected[p])
                 }),
                 collapse = ",\\quad"),
               ") \\)")
      )),
      style = "float:left;width: 150px;padding-left:60px;padding-top:10px;"
    )} else {div()}
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
               div(HTML(paste0("<p style='color:#ff0000'><b>X<sub>",i,"</sub></b></p>")),
                   style = "float:left; padding-right:10px"),
               div(sliderInput(
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





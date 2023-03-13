############################################################
# Slider Maker
############################################################

obsHeaderFun <- function(nVars){tags$p(tags$b(if(nVars == 1){"Draws of Y"} else {"Models (1 draw each)"}))}


obsSliderFun <- function(ns=NULL, nVars){

  titleStr <- if(nVars == 1){"Draws of Y"} else {"Number of Models (1 draw each)"}

  fluidRow(
    tags$p(tags$b("Parameters"), style = "padding-top:15px;padding-left:15px;"),
    column(
      12,
      div(
        div(tags$p(tags$b("n")), style = "color:#ff0000; float:left; padding-right:10px;"),
        div(sliderInput(ns("nObs"),
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












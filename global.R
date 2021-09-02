

############################################################
#
# File for defining global variables and related functions. 
#
############################################################


############################################################
# QOIs
############################################################
selectedQOI <- "Expected Values"

QOIDF <- read.xlsx2("QOIList.xlsx",1, stringsAsFactors = F)
QOIChoices <- QOIDF$Name
############################################################
# Mapping distributions to functions to use
############################################################


selectedDist <- "Bernoulli-Logit-X"
distrDF <- read.xlsx2("DistrNames.xlsx",1, stringsAsFactors = F)


optGroups <- list()

for(g in unique(distrDF$distrGroups)){
  
  distrs <- distrDF$distrList[which(distrDF$distrGroups == g)]
  
  newNames <- c(names(optGroups), g)
  optGroups <- append(optGroups, list(distrs))
  names(optGroups) <- newNames
  
}



############################################
# Switchers
############################################

nVarSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$nVarList[[idx]]))
  return(f)} else(stop("Unknown Distribution!"))
  
}


marginalsChoicesSwitcher <- function(distrID, ...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$marginalsChoicesList[[idx]]))
  return(f)} else(stop("Unknown Distribution!"))
}


paramSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$sliderList[[idx]]))
  return(f)} else(stop("Unknown Distribution!"))
  
}

distrPlot <- function(distrID, ...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$distrPlotList[[idx]]))
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}


MLEPlot <- function(distrID, ...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$MLEList[[idx]]))
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}




dataPrintSwitcher <- function(distrID,...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$dataprintList[[idx]]))
  return(f(...) )} else(stop("Unknown Distribution!"))
  
  
}

drawSwitcher <- function(distrID, ...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$randomDrawList[[idx]]))
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}



latexSwitcher <- function(distrID, ...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <-  eval(parse(text=distrDF$latexList[[idx]]))
  return(f(...) )} else(stop("Unknown Distribution!"))
}



modelSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$randomDrawList[[idx]]))
  return(f )} else(stop("Unknown Distribution!"))
  
}



transformSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$transformFunList[[idx]]))
  return(f )} else(stop("Unknown Distribution!"))
  
}


paramTexLookup <- function(distrID, meta = "F"){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- if(meta){distrDF$metaParamTex[[idx]]} else {distrDF$paramTex[[idx]]}
  return(f )} else(stop("Unknown Distribution!"))
  
}


QOISwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  f <- eval(parse(text=distrDF$QOIList[[idx]]))
  
  div(selectInput(
    inputId = "QOIid", label = div(tags$p(tags$b("Quantity of Interest"),
                                          style = "font-size:15px; !important")),
    choices = f, selected = selectedQOI, width = "200px"),
    style = "padding-top:10px;", class = "simInput")
  
}


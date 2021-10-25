
############################################
# Switchers
############################################
## TODO: just get 1 dataframe, assert the columns, and then read from it
assumedDistrSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  f <- eval(parse(text=distrDF$assumedDistrChoices[[idx]]))
  
  div(selectInput(
    inputId = "assumedDistrID",
    label = tags$p(tags$b("Assumed Distribution"),style = "font-size:15px; !important"),
    choices = f, 
    width = "250px"), class = "distrInput")
  
}

groupSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- distrDF$distrGroups[[idx]]
  return(f)} else(stop("Unknown Distribution!"))
}


nVarSwitcher <- function(distrID){
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$nVar[[idx]]))
  return(f)} else(stop("Unknown Distribution!"))
}


nCovarSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$nCovar[[idx]]))
  return(f)} else(stop("Unknown Distribution!"))
  
}


marginalsChoicesSwitcher <- function(distrID, ...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$marginalsChoicesList[[idx]]))
  return(f)} else(stop("Unknown Distribution!"))
}


paramSwitcher <- function(distrID, type = "param"){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=paste0(distrDF$sliderFun[[idx]], "inputName = '", type, "')")))
  return(f)} else(stop("Unknown Distribution!"))
  
}

distrPlot <- function(distrID, ...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$distrPlotList[[idx]]))
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}

paramNameSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- distrDF$paramName[[idx]]
  return(f )} else(stop("Unknown Distribution!"))
  
}



likelihoodFunSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$likelihoodFun[[idx]]))
  return(f )} else(stop("Unknown Distribution!"))
  
}

chartDomainSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$chartDomain[[idx]]))
  return(f )} else(stop("Unknown Distribution!"))
  
}


optimMethodSwitcher <- function(distrID){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- distrDF$optimMethod[[idx]]
  return(f )} else(stop("Unknown Distribution!"))
  
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
  
  if(length(idx) > 0){div(selectInput(
    inputId = "QOIid", label = div(tags$p(tags$b("Quantity of Interest"),
                                          style = "font-size:15px; !important")),
    choices = f, selected = selectedQOI, width = "200px"),
    style = "padding-top:10px;", class = "simInput")
  } else(stop("Unknown Distribution!"))
  
}



QOIXAxisSwitcher <- function(distrID, type){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(type == "param"){
    if(length(idx) > 0){f <- distrDF$simXAxis_param[[idx]]
    return(f )} else(stop("Unknown Distribution!"))
  } 
  
}

testParamsSwitcher <- function(distrID, ...){
  
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$testParams[[idx]]))
  return(f )} else(stop("Unknown Distribution!"))
  
}

analyticDomainSwitcher <- function(distrID){
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$analyticDomain[[idx]]))
  return(f )} else(stop("Unknown Distribution!"))
  
}

analyticRangeSwitcher <- function(distrID){
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$analyticRange[[idx]]))
  return(f )} else(stop("Unknown Distribution!"))
  
}


pdfSwitcher <- function(distrID){
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- eval(parse(text=distrDF$pdfList[[idx]]))
  return(f )} else(stop("Unknown Distribution!"))
  
}



handMLESwitcher <- function(distrID,...){
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- if(
    distrDF$distrGroups[[idx]] == "Bernoulli" | distrDF$distrGroups[[idx]] == "Ordered Probit (X)" ){
    histAndDensityDiscrete } else {histAndDensity}
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}



functionalFormPlotSwitcher <- function(distrID,...){
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- if(distrDF$distrGroups[[idx]] == "Ordered Probit (X)" ){functionalFormPlotOrdered
  } else {functionalFormPlot}
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}


dataConfigSwitcher <- function(distrID) {
  
  tryCatch({realDataConfig %>%  filter(distrList == distrID)},
           error = function(e){stop("Unknown Distribution!")})
  
}



distrConfigSwitcher <- function(distrID) {
  
  tryCatch({distrDF %>%  filter(distrList == distrID)},
           error = function(e){stop("Unknown Distribution!")})
  
}



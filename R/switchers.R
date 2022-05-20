
############################################
# Switchers
############################################
latexSwitcher <- function(distrID, ...){

  idx <- which(distrDF$distrList==distrID)

  if(length(idx) > 0){f <-  eval(parse(text=distrDF$latexList[[idx]]))
  return(f(...) )} else(stop("Unknown Distribution!"))
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

handMLESwitcher <- function(distrID,...){
  idx <- which(distrDF$distrList==distrID)

  if(length(idx) > 0){f <- if(
    distrDF$distrGroup[[idx]] == "Bernoulli" | distrDF$distrGroup[[idx]] == "Ordered" ){
    histAndDensityDiscrete } else {histAndDensity}
  return(f(...) )} else(stop("Unknown Distribution!"))

}

functionalFormPlotSwitcher <- function(distrID,...){
  idx <- which(distrDF$distrList==distrID)
  
  if(length(idx) > 0){f <- if(distrDF$distrGroup[[idx]] == "Ordered" ){functionalFormPlotOrdered
  } else {functionalFormPlot}
  return(f(...) )} else(stop("Unknown Distribution!"))
  
}

distrConfigSwitcher <- function(distrID) {
  
  tryCatch({distrDF %>%  filter(distrList == distrID)},
           error = function(e){stop("Unknown Distribution!")})
  
}



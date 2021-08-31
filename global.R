

############################################################
#
# File for defining global variables and related functions. 
#
############################################################


############################################################
# independent variables. generated once for each run
############################################################

# menu of X choices
xGenerationChoices <- c("None", "Constant", "Binary", "Uniform", "Normal")

allXNone <- matrix(0, 200, 10)
allXConstant <- matrix(1, 200, 10)

allXBinary <- matrix(rbinom(n = 2000, size = 1, prob = .5), 200, 10)
allXUniform <- matrix(runif(n = 2000, min = 0, max =1), 200, 10)
allXNormal <- matrix(rnorm(n = 2000, mean = 0, sd = 1), 200, 10)

# returns first nRow rows and nCol cols
# where nRow shd be n and nCol shd be k
# first col always 1
xValGenerator <- function(nRow, type=c("Constant")){
  
  # TODO: make extensible to more than 2 cases
  if(!any(is.null(type))){
    cbind(
      allXConstant[1:nRow,1],
      if(length(unique(type)) == 1){
        eval(parse(text = paste0("allX",type[1],"[1:nRow, 1:length(type)]")))
      } else {
        lapply(
          type,
          function(t){
            eval(parse(text = paste0("allX",t,"[1:nRow, 1]")))}) %>% 
          unlist() %>% matrix(nRow, length(type)) 
      })
  } else {allXConstant[1:nRow,1]}
}


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




############################################################
#
# File for defining global variables and related functions. 
#
############################################################


############################################################
# QOIs
############################################################
selectedQOI <- "Expected Values"

QOIDF <- read_excel("QOIList.xlsx",1, stringsAsFactors = F)
QOIChoices <- QOIDF$Name
############################################################
# Mapping distributions to functions to use
############################################################

# TODO: clean up assumed distr choices?
selectedDist <- "Normal-X"
distrDF <- read_excel("DistrNames.xlsx",1, stringsAsFactors = F)


optGroups <- list()

for(g in unique(distrDF$distrGroups)){
  
  distrs <- distrDF$distrList[which(distrDF$distrGroups == g)]
  
  newNames <- c(names(optGroups), g)
  optGroups <- append(optGroups, list(distrs))
  names(optGroups) <- newNames
  
}





############################################################
#
# File for defining global variables and related functions. 
#
############################################################

############################################################
# colors
############################################################
iqOrangeStr <- "#BF5803"
iqBlueStr <- "#3E77BB"
iqGrayStr <- "#2f2f2f"
############################################################
# QOIs
############################################################
selectedQOI <- "Sim. Parameter"

QOIDF <- read_excel("Config/QOIList.xlsx",1)
QOIChoices <- QOIDF$Name
############################################################
# Mapping distributions to functions to use
############################################################

# TODO: clean up assumed distr choices?
selectedDist <- "Bernoulli (Logit, X)"
distrDF <- read_excel("Config/DistrNames.xlsx",1)


optGroups <- list()

for(g in unique(distrDF$distrGroups)){
  
  distrs <- distrDF$distrList[which(distrDF$distrGroups == g)]
  
  newNames <- c(names(optGroups), g)
  optGroups <- append(optGroups, list(distrs))
  names(optGroups) <- newNames
  
}





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
cbPalette <- c("#56B4E9", "#009E73","#E69F00","#0072B2", "#D55E00", "#CC79A7", rep("#999999", 5))
baseColor <- cbPalette[1]
baseColor2 <- cbPalette[2]
baseColor3 <- cbPalette[3]

############################################################
# choices of distribution
############################################################

# TODO: clean up assumed distr choices?
selectedDist <- "Normal (X)"
distrDF <- read_excel("Config/DistrNames.xlsx",1)

optGroups <- list()

for(g in unique(distrDF$distrGroup)){
  
  distrs <- distrDF$distrList[which(distrDF$distrGroup == g)]
  
  newNames <- c(names(optGroups), g)
  optGroups <- append(optGroups, list(distrs))
  names(optGroups) <- newNames
  
}



############################################################
# QOIs
############################################################
selectedQOI <- "Sim. Parameter"

QOIDF <- read_excel("Config/QOIList.xlsx",1)
QOIChoices <- QOIDF$Name

############################################################
# Tutorial Text
############################################################

tutorialText <- read.csv("Config/TutorialInfo.csv")


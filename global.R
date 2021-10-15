

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
# choices of distribution
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



############################################################
# QOIs
############################################################
selectedQOI <- "Sim. Parameter"

QOIDF <- read_excel("Config/QOIList.xlsx",1)
QOIChoices <- QOIDF$Name

############################################################
# Real Data
############################################################
realDataConfig <- read_excel("Config/realDataInfo.xlsx",1)

neumayerData <- readRDS("realData/realDataNeumayer.rds") %>%  tibble()
drehJenData <- readRDS("realData/realDataDJ.rds") %>%  tibble()


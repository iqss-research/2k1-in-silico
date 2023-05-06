## GLOBAL VARIABLES
distrDF <- fst::read_fst(app_sys("DistrNames.fst"), as.data.table=TRUE)
#distrDF <- fst::read_fst("inst/DistrNames.fst", as.data.table=TRUE)

# distrDF <- distrDF %>%
#   mutate(secondParamTex = ifelse(secondParamTex=="\\tau",
#                              "\\tau_1",
#                              secondParamTex))
# fst::write_fst(distrDF, "inst/DistrNames.fst")

#QOIDF <- data.table::fread("inst/QOIList.csv", encoding="UTF-8")
#QOIDF <- data.table::fread(app_sys("QOIList.csv"), encoding="UTF-8")
QOIDF <- data.table::data.table(Name=c("Predicted Values",
                             "Probability Y > 1",
                             "Expected Values",
                             "Sim. Parameter"),
                    FunctionName=c("ycOutput",
                                   "ycGrtOutput",
                                   "expValsOutput",
                                   "paramHistOutput"))
QOIChoices <- QOIDF$Name




optGroups <- list()
for(g in unique(distrDF$distrGroup)){

  distrs <- distrDF$distrList[which(distrDF$distrGroup == g)]

  newNames <- c(names(optGroups), g)
  optGroups <- append(optGroups, list(distrs))
  names(optGroups) <- newNames
}


pkgEnv <- new.env()
pkgEnv$tutorialText <- data.table::fread(app_sys("TutorialText.csv"))


############################################################
# independent variables. generated the same each time
############################################################
# menu of X choices
defaultXChoices <- c("Normal B","Uniform A","Poisson C","Normal A", "Uniform B","Uniform C","Normal C", "Bernoulli A")

xLength <- 800
xWidth <- 1
set.seed(2001)
`allXConstant (1)` <- matrix(1, xLength, xWidth)

`allXBernoulli A` <- matrix(rbinom(n = xWidth*xLength, size = 1, prob = .5), xLength, xWidth)
`allXBernoulli B` <- matrix(rbinom(n = xWidth*xLength, size = 1, prob = .75), xLength, xWidth)
`allXBernoulli C` <- matrix(rbinom(n = xWidth*xLength, size = 1, prob = .25), xLength, xWidth)
`allXUniform A` <- matrix(runif(n = xWidth*xLength, min = 0, max =1), xLength, xWidth)
`allXUniform B` <- matrix(runif(n = xWidth*xLength, min = -1, max =1), xLength, xWidth)
`allXUniform C` <- matrix(runif(n = xWidth*xLength, min = 0, max =2), xLength, xWidth)
`allXNormal A` <- matrix(rnorm(n = xWidth*xLength, mean = 0, sd = 1), xLength, xWidth)
`allXNormal B` <- matrix(rnorm(n = xWidth*xLength, mean = 1, sd = 1), xLength, xWidth)
`allXNormal C` <- matrix(rnorm(n = xWidth*xLength, mean = 0, sd = 2), xLength, xWidth)
`allXPoisson A`<- matrix(rpois(n = xWidth*xLength,lambda = 1), xLength, xWidth)
`allXPoisson B`<- matrix(rpois(n = xWidth*xLength,lambda = 2), xLength, xWidth)
`allXPoisson C`<- matrix(rpois(n = xWidth*xLength,lambda = 3), xLength, xWidth)


selectedDist <- "Normal (X)"
show_getStarted = TRUE
selectedQOI <- "Predicted Values"
paramSliderWidth <- "225px"
xGenerationChoices <- c("Bernoulli A", "Bernoulli B", "Bernoulli C", "Uniform A",
                        "Uniform B", "Uniform C", "Normal A", "Normal B",
                        "Normal C", "Poisson A", "Poisson B", "Poisson C")

regMin <- -5
regMax <- 5
regStep <- .5
regStart <- 3

iqOrangeStr <- "#BF5803"
iqBlueStr <- "#3E77BB"
iqGrayStr <- "#2f2f2f"
cbPalette <- c("#56B4E9", "#009E73","#E69F00","#0072B2", "#D55E00", "#CC79A7", rep("#999999", 5))
baseColor <- cbPalette[1]
baseColor2 <- cbPalette[2]
baseColor3 <- cbPalette[3]


### OPTIONS ########################################

options(warn = -1,
        spinner.color="#9a2b35",
        spinner.size=0.7,
        "launch.browser" = "T")#, shiny.fullstacktrace = T)


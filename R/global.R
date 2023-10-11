## GLOBAL VARIABLES
distrDF <- fst::read_fst(app_sys("DistrNames.fst"), as.data.table=TRUE)

### Adjusting Normal(X) range to get rid of peaks
distrDF[11,20] <- "c(0, 2)"
### Adjusting Exponential (Exp) range
distrDF[6,20] <- "c(0, 8)"
### Adjusting Exponential (Exp, X) range
distrDF[7,20] <- "c(0, 8)"
### Adjusting Log Normal range
distrDF[8,20] <- "c(0, 1.5)"
### Adjusting Log Normal (X) range
distrDF[9,20] <- "c(0, 5)"
### Adjusting Negative Binomial (X) range
distrDF[10,20] <- "c(0, 1)"
### Adjusting Negative Binomial (X) gamma scale from c(1.1, 2)
distrDF[10, 28] <- "c(-1.5, 1.5)"
### Adjusting the tilde over Bernoulli parameter up
distrDF[1, 12] <- "$ \\tilde{E}(y) = \\tilde{\\\\pi} = \\tilde{Pr}(Y=1)$"

#fst::write_fst(distrDF, "inst/DistrNames.fst")
## Periodically write to csv

### Adjust domain for Poisson?
### Adjust domain for Poisson (Exp)?

#distrDF <- fst::read_fst("inst/DistrNames.fst", as.data.table=TRUE)

# original negBinomX sliderStarts: c(.25,.2,.25,0)

# probModelWidth <- c(NA, NA, NA, NA, NA, NA, NA, 1000, 1067, 1051, 1008, 843, 845, 1083, 1113, 1133, 816, 888)
# probModelXsWidth <- c(NA, NA, 1022, 1022, NA, NA, NA, NA, NA, 1026, 1031, 836, 836, NA, NA, NA, NA, 1031)

# modelXsWidth <- c(NA, NA, 870, 871, NA, NA, NA, NA, NA, 865, 872, 850, 848, NA, NA, NA, NA, 870)
# likelihoodTex <- c(NA,966,1040,NA,NA,934,1036,933,975,1400,948,NA,NA,NA,NA,882,801,917)
# loglikelihoodTex <- c(868,NA,NA,NA,NA,NA,832,NA,NA,888,948,1096,1113,NA,NA,NA,NA,NA)
# distrDF <- subset(distrDF, select=-c(likelihoodTex,logLikelihoodTex))
#
# distrDF[ , `:=` (likelihoodTexWid=likelihoodTex,logLikelihoodTexWid=loglikelihoodTex)]
#
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


popify_nosan <- function(el, title, content, placement = "bottom", trigger = "hover", options = NULL) {

  pop = do.call(shinyBS::popify, args = list(el, title, content, placement, trigger, options))

  pop[[2]]$children[[1]][[1]] = gsub("shinyBS.addTooltip", "addTooltip_sanitize", pop[[2]]$children[[1]][[1]])

  return(pop)
}

### OPTIONS ########################################

options(warn = -1,
        spinner.color="#9a2b35",
        spinner.size=0.7,
        "launch.browser" = "T")#, shiny.fullstacktrace = T)


neumayerPDF <- neumayerParamTransform <- neumayerPlotDistr <- neumayerLikelihoodFun <- neumayerLatex <- function(...){NULL}
drehJenPDF <- drehJenParamTransform <- drehJenPlotDistr <- drehJenLikelihoodFun <- drehJenLatex <- function(...){NULL}
  
singleChartDomain <- list(from = .01,to = 1,by = .01)
neumayerChartDomain <- list(singleChartDomain)
drehJensChartDomain <- list(singleChartDomain)



neumayerDraws <- function(param, nObs){readRDS("realData/realDataNeumayer.rds")$multish}
drehJenDraws <- function(param, nObs){readRDS("realData/realDataDJ.rds")$un_per_l}


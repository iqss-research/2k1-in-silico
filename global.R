

############################################################
#
# File for defining a few global variables. 
#
############################################################



xParamBase <- rnorm(10, 5, 2)
indepVarsBase <- sapply(xParamBase, function(a){rnorm(200, a, 2)})
indepVarsBase[,1] <- 1




############################################################
# Mapping distributions to functions to use
############################################################


selectedDist <- "Log-Normal-X"

distrList <- list(
  "Bernoulli-Pi",
  "Bernoulli-Logit",
  "Bernoulli-Logit-X",
  "Stylized-Normal" ,
  "Stylized-Normal-X",
  "Log-Normal",
  "Log-Normal-X",
  "Poisson",
  "Poisson-Exponential",
  "Exponential",
  "Exponential-Exponential"
)

distrGroups <- list(
  "Bernoulli",
  "Bernoulli",
  "Bernoulli",
  "Normal",
  "Normal",
  "Log-Normal",
  "Log-Normal",
  "Poisson",
  "Poisson",
  "Exponential",
  "Exponential"
)

optGroups <- list()

for(g in unique(distrGroups)){
  
  distrs <- distrList[which(distrGroups == g)]
  
  newNames <- c(names(optGroups), g)
  optGroups <- append(optGroups, list(distrs))
  names(optGroups) <- newNames
  
}



sliderList <- list(
  bernSlider,
  bernLogitSlider,
  bernLogitXSlider,
  styNormSlider,
  multiNormSlider,
  logNormSlider,
  logNormXSlider,
  poisSlider,
  poisExpSlider,
  expSlider,
  expExpSlider
)


nVarList <- list(
  1,
  1,
  3,
  1,
  3,
  1,
  3,
  1,
  1,
  1
)

marginalsChoicesList <- list(
  c(),
  c(),
  c("Beta0", "Beta1", "Beta2"),
  c(),
  c("Beta0", "Beta1", "Beta2"),
  c(),
  c("Beta0", "Beta1", "Beta2"),
  c(),
  c(),
  c(),
  c()
)


distrPlotList <- list(
  bernPlotDistr,
  bernLogitPlotDistr,
  bernLogitXPlotDistr,
  styNormPlotDistr,
  multiNormPlotDistr,
  logNormPlotDistr,
  logNormXPlotDistr,
  poisPlotDistr,
  poisExpPlotDistr,
  expPlotDistr,
  expExpPlotDistr
)

MLEList <- list(
  function(a, margNum){MLEPlotter(a, bernChartDomain, bernLikelihoodFun, "Pi")},
  function(a, margNum){MLEPlotter(a, bernLogitChartDomain, bernLogitLikelihoodFun, "Beta")},
  function(a, margNum){MLEPlotter(a, bernLogitXChartDomain, bernLogitXLikelihoodFun, "Beta", margNum)},
  function(a, margNum){MLEPlotter(a, styNormChartDomain, styNormLikelihoodFun, "Beta")},
  function(a, margNum){MLEPlotter(a, multiNormChartDomain, multiNormLikelihoodFun, "Beta", margNum)},
  function(a, margNum){MLEPlotter(a, logNormChartDomain, logNormLikelihoodFun, "Beta")},
  function(a, margNum){MLEPlotter(a, logNormXChartDomain, logNormXLikelihoodFun, "Beta", margNum)},
  function(a, margNum){MLEPlotter(a, poisChartDomain, poisLikelihoodFun, "Lambda")},
  function(a, margNum){MLEPlotter(a, poisExpChartDomain, poisExpLikelihoodFun, "Beta")},
  function(a, margNum){MLEPlotter(a, expChartDomain, expLikelihoodFun, "Lambda")},
  function(a, margNum){MLEPlotter(a, expExpChartDomain, expExpLikelihoodFun, "Beta")}
)

dataprintList <- list(
  intPrintHelper,
  intPrintHelper,
  intPrintHelper,
  decPrintHelper,
  decPrintHelper,
  decPrintHelper,
  decPrintHelper,
  intPrintHelper,
  intPrintHelper,
  decPrintHelper,
  decPrintHelper
)


randomDrawList <- list(
  bernDraws,
  bernLogitDraws,
  bernLogitXDraws,
  styNormDraws,
  multiNormDraws,
  logNormDraws,
  logNormXDraws,
  poisDraws,
  poisExpDraws,
  expDraws,
  expExpDraws
)

latexList <- list(
  bernLatex,
  bernLogitLatex,
  bernLogitXLatex,
  styNormLatex,
  multiNormLatex,
  logNormLatex,
  logNormXLatex,
  poisLatex,
  poisExpLatex,
  expLatex,
  expExpLatex
)


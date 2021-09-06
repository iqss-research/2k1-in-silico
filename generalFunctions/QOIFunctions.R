############################################################
# Every function on this page should take one argument,
# y tilde (simulated y). 
#
# The function should return a ggplot object and a table
# Either can be empty. 
#
# The function name should be listed with the QOI name
# in QOIList.xlsx
############################################################

### TODO clean up args

ycOutput <- function(yTilde, muTilde, distrID){
  ciInt <- if(length(unique(yTilde)) > 2){ quantile(yTilde, c(.1, .9))} else {NULL}
  
  histogramMaker(yTilde, title = "Predicted Values of Y", annotate = T,
                 ci = ciInt, border = F)}

ycGrtOutput <- function(yTilde, muTilde, distrID){
  histogramMaker(yTilde, title = "Predicted Values of Y", greaterThan = 1)}

paramHistOutput <- function(yTilde, muTilde, distrID){
  histogramMaker(muTilde, title = "Simulated Values of Parameter", greaterThan = 1)}


expValsOutput <- function(yTilde, muTilde, distrID){
  
  expVals <- expValCreator(muTilde, modelSwitcher(distrID))
  xAxis <- QOIXAxisSwitcher(distrID, "param")
  
  histogramMaker(expVals, title = xAxis, annotate = T,
                 ci = quantile(expVals, c(.1, .9)), border = F
                 )
  
}

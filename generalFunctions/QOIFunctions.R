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
  intervalBottom <- quantile(yTilde, .025)
  intervalTop <- quantile(yTilde, .975)
  tmpStr <- paste0("95% Confidence Interval: (", round(intervalBottom, 2),", ",round(intervalTop,2),")" )
  
  histogramMaker(yTilde, title = "Predicted Values of Y", annotate = T, captionText = tmpStr)}

ycGrtOutput <- function(yTilde, muTilde, distrID){
  histogramMaker(yTilde, title = "Predicted Values of Y", greaterThan = 1)}

expValsOutput <- function(yTilde, muTilde, distrID){
  
  expVals <- expValCreator(muTilde, modelSwitcher(distrID))
  
  intervalBottom <- quantile(expVals, .025)
  intervalTop <- quantile(expVals, .975)
  tmpStr <- paste0("95% Confidence Interval: (", round(intervalBottom, 2),", ",round(intervalTop,2),")" )
  
  histogramMaker(expVals, title = "Expected Values of Y", annotate = T, captionText = tmpStr)
  
}

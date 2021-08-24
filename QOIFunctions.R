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



ycOutput <- function(yTilde,muTilde, distrID){histogramMaker(yTilde, title = "Predicted Values of Y")}

ycGrtOutput <- function(yTilde, muTilde, distrID){histogramMaker(yTilde, title = "Predicted Values of Y", greaterThan = 1)}

mucOutput <- function(yTilde, muTilde, distrID){histogramMaker(muTilde, title = muTitleLookup(distrID))}

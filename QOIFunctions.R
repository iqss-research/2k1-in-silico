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



ycOutput <- function(yTilde){histogramMaker(yTilde, title = "Simulated Y")}


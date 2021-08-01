

############################################################
#
# File for defining a few global variables. 
#
############################################################



xParamBase <- rnorm(10, 5, 2)
indepVarsBase <- sapply(xParamBase, function(a){rnorm(200, a, 2)})




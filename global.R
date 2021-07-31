

############################################################
#
# File for defining a few global variables. 
#
############################################################



xParamBase <- rnorm(10, 3, 1)
indepVarsBase <- sapply(xParamBase, function(a){rnorm(200, a, 1)})




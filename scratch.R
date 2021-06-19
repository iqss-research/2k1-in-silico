library(ggplot2)
library(magrittr)
library(ADtools)

source("BernoulliHelpers.R")
source("styNormHelpers.R")

likelihoodFun <- styNormLikelihoodFun

truebeta <- 2
nObs <- 500
outcome <- styNormDraws(truebeta, nObs)




result <- optim(par = .5, likelihoodFun, outcome = outcome, hessian = TRUE, control = list(fnscale = -1))
result$hessian
piHat <- result$par
piSE <- solve(-1*result$hessian) %>%  sqrt()

chartDomain <- -4999:4999/1000
# likelihoodFun(chartDomain, nSuccesses, nObs)


testData <- data.frame(x = chartDomain, y = sapply(chartDomain, function(a) likelihoodFun(a, outcome)), z= result$hessian*(chartDomain-piHat)^2/2 + likelihoodFun(piHat, outcome))

# testData$x[which(abs(dLdpi - likelihoodFun(piHat, outcomeNorm)) < 1e-2)]

ggplot() + geom_line(data = testData, mapping = aes(x = x, y = y), color = "steelblue") + geom_line(data = testData, mapping = aes(x = x, y = z))



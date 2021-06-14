library(ggplot2)
library(magrittr)
library(ADtools)

source("BernoulliHelpers.R")

likelihoodFun <- function(testPiParam, nSuccesses, nVal){
  log((testPiParam^nSuccesses)*((1-testPiParam)^(nVal-nSuccesses)))
}

truePi <- .6
nObs <- 500
outcome <- bernDraws(truePi, nObs)
nSuccesses <- sum(outcome)




result <- optim(par = .5, likelihoodFun, nSuccesses = nSuccesses, nVal = nObs, hessian = TRUE, control = list(fnscale = -1))
result$hessian
solve(-1*result$hessian) %>%  sqrt()

piHat <- result$par
piSE <- solve(-1*result$hessian) %>%  sqrt()

chartDomain <- 2:999/1000
# likelihoodFun(chartDomain, nSuccesses, nObs)


testData <- data.frame(x = chartDomain, y = likelihoodFun(chartDomain, nSuccesses, nObs), z= result$hessian*(chartDomain-piHat)^2 + likelihoodFun(piHat, nSuccesses, nObs))

# testData$x[which(abs(dLdpi - likelihoodFun(piHat, outcomeNorm)) < 1e-2)]

ggplot() + geom_line(data = testData, mapping = aes(x = x, y = y), color = "steelblue") + geom_line(data = testData, mapping = aes(x = x, y = z))



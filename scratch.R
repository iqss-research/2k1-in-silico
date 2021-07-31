
QApprox2 <- c()

for(i in 1:nrow(diffMat)){
  
  tmpVec <- diffMat[i,]
  
  QApprox2 <- c(QApprox2, t(tmpVec) %*% optimizer$hessian %*%  tmpVec)
  
}
QApprox <- QApprox2  + likelihoodFun(paramHat,outcome)



testParam <- paramHat


dimN <- length(testParam)

(-1/2)*sum((outcome-testParam)^2)


inx <- which(likelihoodAll$LogLikelihood == max(likelihoodAll$LogLikelihood))
inx2 <- which(QApprox == max(QApprox))


likelihoodAll$LogLikelihood[[inx]]
QApprox[[inx]]

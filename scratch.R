params <- c(1,-1, 0)
outcome <- multiNormDraws(params, 20)
margNum <- 1


chartDomain <- multiNormChartDomain
likelihoodFun <- multiNormLikelihoodFun

testParams <- rep(.5, ncol(chartDomain))

optimizer <- optim(par = testParams, likelihoodFun, hessian = TRUE, control = list(fnscale = -1), outcome = outcome)
paramHatRaw <- optimizer$par
paramHessian <- optimizer$hessian
paramSE <- diag(solve(-1*optimizer$hessian) %>%  sqrt())
paramHatMatrix <- matrix(rep(paramHatRaw, nrow(chartDomain)), ncol = ncol(chartDomain), byrow = T)
diffMat <- (chartDomain %>%  as.matrix() )- paramHatMatrix
###########################################
QApproxOld <- c()
for(i in 1:nrow(diffMat)){

  tmpVec <- diffMat[i,]
  QApproxOld <- c(QApproxOld, .5*(t(tmpVec) %*% optimizer$hessian %*%  tmpVec))

}
QApproxOld <- QApproxOld  + likelihoodFun(paramHatRaw,outcome)

LLOld <- generalMleFun(outcome,chartDomain, likelihoodFun) %>%  select(LogLikelihood)
(QApproxOld - LLOld)/LLOld[,1] %>% head()


#################################################


minIdx <- lapply(seq_len(ncol(diffMat)), function(i) which.min(abs(diffMat[,i]))) %>%  unlist()
paramHat <- diag(chartDomain[minIdx,] %>%  as.matrix())
chartDomainSmall <- chartDomain


margRemoveCols <- (1:ncol(chartDomainSmall))[which(1:ncol(chartDomainSmall) != margNum)]
for(j in (1:ncol(chartDomainSmall))[margRemoveCols]){
  chartDomainSmall <- chartDomainSmall %>%  filter_at(c(j), all_vars(.==paramHat[[j]]))}

paramHatMatrixSmall <-  matrix(rep(paramHatRaw, nrow(chartDomainSmall)), ncol = ncol(chartDomainSmall), byrow = T)
diffMatSmall <- (chartDomainSmall %>%  as.matrix() )- paramHatMatrixSmall


QApproxNew <- c()
for(i in 1:nrow(diffMatSmall)){
  
  tmpVec <- diffMatSmall[i,]
  QApproxNew <- c(QApproxNew, .5*(t(tmpVec) %*% optimizer$hessian %*%  tmpVec))
  
}
QApproxNew <- QApproxNew  + likelihoodFun(paramHatRaw,outcome)

LLNew <- generalMleFun(outcome,chartDomainSmall, likelihoodFun) %>%  select(LogLikelihood)
(QApproxNew - LLNew)/LLNew[,1] %>% head()

data.frame(param = chartDomainSmall[,margNum],LogLikelihood = LLNew, QuadraticApprox= QApproxNew)

#####################################################



if(length(margNum) == 0){margNum <- 1}

xAxisName <- paste0("Parameter ", paramName)
nParam <- ncol(chartDomain)

qApprox <- quadraticLikelihoodApprox(likelihoodFun = likelihoodFun, chartDomain = chartDomain,
                                     testParams = rep(.5, nParam), margNum = margNum, outcome = outcome)
likelihoodDB <- qApprox$data
paramHat <- qApprox$paramHat

colnames(likelihoodDB) <- c("param", "LogLikelihood", "QuadraticApprox")

# charting begins here

ggplot() + 
  geom_line(data = likelihoodDB, mapping =  aes(x = param, y = LogLikelihood), color = "steelblue", size = 1) + 
  theme_minimal() +
  xlab(xAxisName) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
  ) + geom_line(data = likelihoodDB, mapping =  aes(x = param, y = QuadraticApprox), color = "firebrick4", size = 1)


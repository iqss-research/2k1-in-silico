

############################################################
# independent variables. generated once for each run
############################################################

defaultXChoices <- c("Uniform(0,1)","Normal(0,1)","Poisson(1)","Normal(0,1)", "Uniform(0,1)","Uniform(0,1)","Normal(0,1)","Uniform(0,1)","Normal(0,1)","Bernoulli(.5)")

xLength <- 800
xWidth <- 20
# menu of X choices

allXNone <- matrix(0, xLength, xWidth)
`allXConstant (1)` <- matrix(1, xLength, xWidth)

`allXBernoulli(.5)` <- matrix(rbinom(n = xWidth*xLength, size = 1, prob = .5), xLength, xWidth)
`allXUniform(0,1)` <- matrix(runif(n = xWidth*xLength, min = 0, max =1), xLength, xWidth)
`allXNormal(0,1)` <- matrix(rnorm(n = xWidth*xLength, mean = 0, sd = 1), xLength, xWidth)
`allXPoisson(1)`<- matrix(rpois(n = xWidth*xLength,lambda = 1), xLength, xWidth)


# write.csv(x = cbind(`allXBernoulli(.5)`,`allXUniform(0,1)`,`allXNormal(0,1)`, `allXPoisson(1)`), file = "xVals.csv")
# FOR NOW: only works for biv case
correlatedX <- function(nRow, rho = 0.75){
  mu1 <- 0; s1 <- 1
  mu2 <- 0; s2 <- 1
  
  mu <- c(mu1,mu2) # Mean
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) #VCov 
  
  biv <- mvrnorm(nRow, mu = c(mu1, mu2), Sigma = sigma )
  cbind(`allXConstant (1)`[1:nRow,1], biv)
}



# returns first nRow rows and nCol cols
# where nRow shd be n and nCol shd be k
# first col always 1
xValGenerator <- function(nRow, type=c("Bernoulli(.5)")){
  
  nType <- length(type)
  # TODO: make extensible to more than 2 cases
  if(!any(is.null(type))){
    cbind(
      `allXConstant (1)`[1:nRow,1],
        lapply(
          1:nType,
          function(i){
            eval(parse(text = paste0("`allX",type[i],"`[1:nRow, ",i,"]")))}) %>% 
          unlist() %>% matrix(nRow, length(type)) 
      )
  } else {`allXConstant (1)`[1:nRow,1]}
}


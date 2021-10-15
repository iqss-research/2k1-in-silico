

############################################################
# independent variables. generated once for each run
############################################################

xLength <- 800
# menu of X choices

allXNone <- matrix(0, xLength, 3)
`allXConstant (1)` <- matrix(1, xLength, 3)

`allXBernoulli(.5)` <- matrix(rbinom(n = 3*xLength, size = 1, prob = .5), xLength, 3)
`allXUniform(0,1)` <- matrix(runif(n = 3*xLength, min = 0, max =1), xLength, 3)
`allXNormal(0,1)` <- matrix(rnorm(n = 3*xLength, mean = 0, sd = 1), xLength, 3)
`allXPoisson(1)`<- matrix(rpois(n = 3*xLength,lambda = 1), xLength, 3)


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
  
  # TODO: make extensible to more than 2 cases
  if(!any(is.null(type))){
    cbind(
      `allXConstant (1)`[1:nRow,1],
      if(length(unique(type)) == 1){
        eval(parse(text = paste0("`allX",type[1],"`[1:nRow, 1:length(type)]")))
      } else {
        lapply(
          type,
          function(t){
            eval(parse(text = paste0("`allX",t,"`[1:nRow, 1]")))}) %>% 
          unlist() %>% matrix(nRow, length(type)) 
      })
  } else {`allXConstant (1)`[1:nRow,1]}
}


#' xFunctions
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd



############################################################
# independent variables. generated the same each time
############################################################
# menu of X choices
defaultXChoices <- c("Normal B","Uniform A","Poisson C","Normal A", "Uniform B","Uniform C","Normal C", "Bernoulli A")

xLength <- 800
xWidth <- 1
set.seed(2001)
`allXConstant (1)` <- matrix(1, xLength, xWidth)

`allXBernoulli A` <- matrix(rbinom(n = xWidth*xLength, size = 1, prob = .5), xLength, xWidth)
`allXBernoulli B` <- matrix(rbinom(n = xWidth*xLength, size = 1, prob = .75), xLength, xWidth)
`allXBernoulli C` <- matrix(rbinom(n = xWidth*xLength, size = 1, prob = .25), xLength, xWidth)
`allXUniform A` <- matrix(runif(n = xWidth*xLength, min = 0, max =1), xLength, xWidth)
`allXUniform B` <- matrix(runif(n = xWidth*xLength, min = -1, max =1), xLength, xWidth)
`allXUniform C` <- matrix(runif(n = xWidth*xLength, min = 0, max =2), xLength, xWidth)
`allXNormal A` <- matrix(rnorm(n = xWidth*xLength, mean = 0, sd = 1), xLength, xWidth)
`allXNormal B` <- matrix(rnorm(n = xWidth*xLength, mean = 1, sd = 1), xLength, xWidth)
`allXNormal C` <- matrix(rnorm(n = xWidth*xLength, mean = 0, sd = 2), xLength, xWidth)
`allXPoisson A`<- matrix(rpois(n = xWidth*xLength,lambda = 1), xLength, xWidth)
`allXPoisson B`<- matrix(rpois(n = xWidth*xLength,lambda = 2), xLength, xWidth)
`allXPoisson C`<- matrix(rpois(n = xWidth*xLength,lambda = 3), xLength, xWidth)


# write.csv(x = cbind(`allXBernoulli A`, `allXBernoulli B`, `allXBernoulli C`, `allXUniform A`,
#                       `allXUniform B`, `allXUniform C`, `allXNormal A`, `allXNormal B`,
#                       `allXNormal C`, `allXPoisson A`, `allXPoisson B`, `allXPoisson C`), file = "xVals.csv")
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
xValGenerator <- function(nRow, type=c("Bernoulli A")){

  nType <- length(type)
  # TODO: make extensible to more than 2 cases
  if(!any(is.null(type))){
    cbind(
      `allXConstant (1)`[1:nRow,1],
      lapply(
        1:nType,
        function(i){
          eval(parse(text = paste0("`allX",type[i],"`[1:nRow, 1]")))}) %>%
        unlist() %>% matrix(nRow, length(type))
    )
  } else {`allXConstant (1)`[1:nRow,1]}
}


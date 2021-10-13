
# my understanding of the gamma PDF with Gary notation

dGammaDirect <- function(lambda, phi, sigma){
  shape <- phi/(sigma^2 -1)
  scale <- sigma^2 -1
  
  dgamma(x = lambda, shape = shape, scale = scale)
}




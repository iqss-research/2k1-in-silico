.onLoad <- function(libname, pkgname){
  library(ggplot2)
  data(sysdata, package = pkgname, envir=parent.env(environment()))
  
}

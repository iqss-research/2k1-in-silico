.onLoad <- function(libname, pkgname){
  library(ggplot)
  data(sysdata, package = pkgname, envir=parent.env(environment()))
  
}

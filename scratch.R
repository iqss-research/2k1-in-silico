packages <- c("MASS","shiny",  "shinythemes", "shinyBS", "shinyjs", "tidyverse", "DT", "bslib", "dashboardthemes", "shinyjs", "calculus", "latex2exp", "xlsx", "mvtnorm", "data.table","DT", "cowplot", "stringr")


dependencyList <- lapply(packages, function(a){
  print(a)
  lst <- miniCRAN::pkgDep(a)
  lst <- lst[stringr::str_order(lst)]
  print(lst)
  lst})


lapply(dependencyList, function(a){any(a == "rJava")})

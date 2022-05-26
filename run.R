# library(lattice)
# library(nlme)
# library(Matrix)
# library(MASS)
# library(mgcv)
# library(shiny)
# library(shinythemes)
# library(shinyBS)
# library(shinyjs)
# library(dplyr)
# library(ggplot2)
# library(bslib)
# library(grid)
# library(dashboard)
# library(dashboardthemes)
# library(calculus)
# 
# library(latex2exp)
# library(readxl)
# library(mvtnorm)
# library(data.table)
# library(DT)
# library(cowplot)
# library(stringr)
# library(VGAM)
# library(reshape2)
# library(rintrojs)


# obtain port argument (if given)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  port <- "12000"
} else if (length(args) == 1) {
  port <- args[1]
}

print(paste("Listening on port", port))

shiny::runApp(
  appDir = paste0(getwd()),
  host = "0.0.0.0",
  port = as.numeric(port)
)
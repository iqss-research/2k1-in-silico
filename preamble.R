############################################################
# Load packages
############################################################

packages <- c("shiny", "shinythemes", "shinyBS", "shinyjs", "dplyr", "tidyr", "ggplot2", "DT", "bslib", "ADtools", "grid", "dashboardthemes")

oldw <- getOption("warn")
options(warn = -1)

package.check <- lapply(packages,FUN = function(x) {
  if (!require(x, character.only = TRUE)) {install.packages(x, dependencies = TRUE)}})

package.load <- lapply(packages, function(x){library(x, character.only = TRUE)})
options(warn = oldw)

############################################################
# Source Files; global vars
############################################################

sapply(list.files("DistributionSpecific/"), function(a)(source(paste0("DistributionSpecific/", a))))
source("generalHelpers.R")
source("ui.R")


if(exists("outcomeData")){rm(outcomeData, envir = .GlobalEnv)}





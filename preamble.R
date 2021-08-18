############################################################
# Load packages
############################################################

packages <- c("shiny", "shinythemes", "shinyBS", "shinyjs", "dplyr", "tidyr", "ggplot2", "DT", "bslib", "ADtools", "grid", "dashboardthemes", "shinyjs", "calculus", "latex2exp", "xlsx", "mvtnorm", "data.table","DT")


package.check <- lapply(packages,FUN = function(x) {
  if (!require(x, character.only = TRUE)) {install.packages(x, dependencies = TRUE)}})

package.load <- lapply(packages, function(x){library(x, character.only = TRUE)})

############################################################
# Source Files; global vars
############################################################

sapply(list.files("DistributionSpecific/"), function(a)(source(paste0("DistributionSpecific/", a))))
source("generalFunctions.R")
source("MLEFunctions.R")
source("simFunctions.R")
source("latex.R")
source("global.R")
source("ui.R")


if(exists("outcomeData")){rm(outcomeData, envir = .GlobalEnv)}





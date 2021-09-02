############################################################
# Load packages
############################################################

packages <- c("shiny", "shinythemes", "shinyBS", "shinyjs", "dplyr", "tidyr", "ggplot2", "DT", "bslib", "ADtools", "grid", "dashboardthemes", "shinyjs", "calculus", "latex2exp", "xlsx", "mvtnorm", "data.table","DT", "cowplot")


package.check <- lapply(packages,FUN = function(x) {
  if (!require(x, character.only = TRUE)) {install.packages(x, dependencies = TRUE)}})

package.load <- lapply(packages, function(x){library(x, character.only = TRUE)})


############################################################
# param slider width
############################################################
paramSliderWidth <- "200px"

############################################################
# Source Files; global vars
############################################################
source("global.R")
sapply(list.files("generalFunctions/"), function(a)(source(paste0("generalFunctions/", a))))
sapply(list.files("DistributionSpecific/"), function(a)(source(paste0("DistributionSpecific/", a))))
source("ui.R")


if(exists("outcomeData")){rm(outcomeData, envir = .GlobalEnv)}





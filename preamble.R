############################################################
# Load packages
############################################################

packages <- c("MASS","shiny",  "shinythemes", "shinyBS", "shinyjs", "tidyverse", "DT", "bslib", "grid", "dashboardthemes", "shinyjs", "calculus", "latex2exp", "readxl", "mvtnorm", "data.table","DT", "cowplot", "stringr", "dashboard")

#TODO: remove for packrat
package.check <- lapply(packages,FUN = function(x) {
  if (!require(x, character.only = TRUE)) {install.packages(x, dependencies = TRUE)}})

# package.load <- lapply(packages, function(x){library(x, character.only = TRUE)})

library(MASS)
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(tidyverse)
library(ggplot2)
library(DT)
library(bslib)
library(grid)
library(dashboard)
library(dashboardthemes)
library(shinyjs)
library(calculus)
library(latex2exp)
library(readxl)
library(mvtnorm)
library(data.table)
library(DT)
library(cowplot)
library(stringr)

set.seed(2001)

############################################################
# param slider width`x`
############################################################
paramSliderWidth <- "225px"
xGenerationChoices <- c("None", "Bernoulli(.5)", "Uniform(0,1)", "Normal(0,1)", "Poisson(1)")


############################################################
# Source Files; global vars
############################################################
source("global.R")
sapply(list.files("generalFunctions/"), function(a)(source(paste0("generalFunctions/", a))))
sapply(list.files("DistributionSpecific/"), function(a)(source(paste0("DistributionSpecific/", a))))
source("ui.R")


if(exists("outcomeData")){rm(outcomeData, envir = .GlobalEnv)}





## GLOBAL VARIABLES
distrDF <- fst::read_fst(app_sys("DistrNames.fst"), as.data.table=TRUE)

#distrDF <- fst::read_fst("inst/DistrNames.fst", as.data.table=TRUE)

### Can I delete these once I save to the fst file?
### Adjusting Normal(X) range to get rid of peaks
distrDF[11,20] <- "c(0, 0.6)"
### Adjusting Exponential (Exp) range
distrDF[6,20] <- "c(0, 1.5)"
### Adjusting Exponential (Exp, X) range
distrDF[7,20] <- "c(0, 1.5)"
### Adjusting Log Normal range
distrDF[8,20] <- "c(0, 1.5)"
### Adjusting Log Normal (X) range
distrDF[9,20] <- "c(0, 1.5)"
### Adjusting Negative Binomial (X) range
distrDF[10,20] <- "c(0, 0.5)"
### Adjusting Negative Binomial (X) gamma scale from c(1.1, 2)
distrDF[10, 28] <- "c(-1.5, 1.5)"


#fst::write_fst(distrDF, "inst/DistrNames.fst")
## Periodically write to csv

### Adjust domain for Poisson?
### Adjust domain for Poisson (Exp)?


# original negBinomX sliderStarts: c(.25,.2,.25,0)

# probModelWidth <- c(NA, NA, NA, NA, NA, NA, NA, 1000, 1067, 1051, 1008, 843, 845, 1083, 1113, 1133, 816, 888)
# probModelXsWidth <- c(NA, NA, 1022, 1022, NA, NA, NA, NA, NA, 1026, 1031, 836, 836, NA, NA, NA, NA, 1031)

# modelXsWidth <- c(NA, NA, 870, 871, NA, NA, NA, NA, NA, 865, 872, 850, 848, NA, NA, NA, NA, 870)
# likelihoodTex <- c(NA,966,1040,NA,NA,934,1036,933,975,1400,948,NA,NA,NA,NA,882,801,917)
# loglikelihoodTex <- c(868,NA,NA,NA,NA,NA,832,NA,NA,888,948,1096,1113,NA,NA,NA,NA,NA)
# distrDF <- subset(distrDF, select=-c(likelihoodTex,logLikelihoodTex))
#
# distrDF[ , `:=` (likelihoodTexWid=likelihoodTex,logLikelihoodTexWid=loglikelihoodTex)]
#
# fst::write_fst(distrDF, "inst/DistrNames.fst")

#QOIDF <- data.table::fread("inst/QOIList.csv", encoding="UTF-8")
#QOIDF <- data.table::fread(app_sys("QOIList.csv"), encoding="UTF-8")
QOIDF <- data.table::data.table(Name=c("Predicted Values",
                             "Probability Y > 1",
                             "Expected Values",
                             "Sim. Parameter"),
                    FunctionName=c("ycOutput",
                                   "ycGrtOutput",
                                   "expValsOutput",
                                   "paramHistOutput"))
QOIChoices <- QOIDF$Name




optGroups <- list()
for(g in unique(distrDF$distrGroup)){

  distrs <- distrDF$distrList[which(distrDF$distrGroup == g)]

  newNames <- c(names(optGroups), g)
  optGroups <- append(optGroups, list(distrs))
  names(optGroups) <- newNames
}


pkgEnv <- new.env()
pkgEnv$tutorialText <- data.table::fread(app_sys("TutorialText.csv"))


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


selectedDist <- "Normal (X)"
show_getStarted = TRUE
selectedQOI <- "Predicted Values"
paramSliderWidth <- "225px"
xGenerationChoices <- c("Bernoulli A", "Bernoulli B", "Bernoulli C", "Uniform A",
                        "Uniform B", "Uniform C", "Normal A", "Normal B",
                        "Normal C", "Poisson A", "Poisson B", "Poisson C")

regMin <- -5
regMax <- 5
regStep <- .5
regStart <- 3

iqOrangeStr <- "#BF5803"
iqBlueStr <- "#3E77BB"
iqGrayStr <- "#2f2f2f"
cbPalette <- c("#56B4E9", "#009E73","#E69F00","#0072B2", "#D55E00", "#CC79A7", rep("#999999", 5))
baseColor <- cbPalette[1]
baseColor2 <- cbPalette[2]
baseColor3 <- cbPalette[3]


popify_nosan <- function(el, title, content, placement = "bottom", trigger = "hover", options = NULL) {

  pop = do.call(shinyBS::popify, args = list(el, title, content, placement, trigger, options))

  pop[[2]]$children[[1]][[1]] = gsub("shinyBS.addTooltip", "addTooltip_sanitize", pop[[2]]$children[[1]][[1]])

  return(pop)
}

helptext <- reactive(data.table(
  tab = c(rep("dgp", 12), rep("Likelihood", 12), rep("QOI", 9), rep("Intro", 6)),
  step = c(1:12, 1:12, 1:9, 1:6),
  element = c('#dgp_step1',
              "#dgp_step2",
              "#dgp_step3",
              "#dgp_step4",
              "#dgp_step5",
              "#dgp_step6",
              "#dgp_step7",
              "#dgp_step8",
              "#dgp_step9",
              "#dgp_step10",
              "#dgp_step11",
              "#dgp_step12",
              '#mod_step1',
              "#mod_step2",
              "#mod_step3",
              "#mod_step4",
              "#mod_step5",
              "#mod_step6",
              "#mod_step7",
              "#mod_step8",
              "#mod_step9",
              "#mod_step10",
              "#mod_step11",
              "#mod_step12",
              '#qoi_step1',
              "#qoi_step2",
              "#qoi_step3",
              "#qoi_step4",
              "#qoi_step5",
              "#qoi_step6",
              "#qoi_step7",
              "#qoi_step8",
              "#qoi_step9",
              '#intro_step1',
              "#titleDiv",
              "#dgp_tab_1-distrNameOutput",
              "#model_tab_1-assumedDistrNameOutput",
              "#qoi_tab_1-simTitleOutput",
              '#MI'),
  intro = c("This is tutorial mode, a guided walk-through of 2K1 In Silico. If you are already familiar with the app, you can use the exit button above.",
            "Choose your data generating model here. We have 6 families and 18 model types available to choose from.",
            "Here we have the probability model, based on our chosen Data Generation Process. This mathematical expression details how our outcome variable y is related to our covariates and parameters.",
            "These are Informational Pop-Ups. Click on them to reveal more information about a specific section.",
            "Here, you can select how much data you want to generate, up to 200 observations.",
            "If your DGP allows you to select covariates, you can do so here. These are variables that help to generate your outcome data. You can use the + button to add covariates, and the - button to remove covariates. Click on X1, X2, etc. to see the underlying distribution of the covariate.",
            "Here you can select your parameters. These include the selected distribution's parameters, as well as the beta coefficients that help to relate the covariates to the outcome variable.",
            "Here are the generated data, Y. These are produced according to the probability model defined above, using your selected DGP, covariates, and parameters.",
            "This is a plot which shows the probability density of your outcome variable, conditional on your parameters.",
            "If you selected an ordinal DGP, this is a plot which shows you the distribution of the unobserved variable.",
            "This plot shows you the distribution of your intermediate parameter, based on your covariates and your parameter choices. This represents the systematic part of the data generation process.",
            "This plot shows you how your covariate relates to the intermediate parameter. If you have more than one covariate, use the dropdown below the X Axis to select which covariate's relationship you would like to see.",
            "This is tutorial mode, a guided walk-through of 2k1 In Silico. If you are already familiar with the app, you can use the exit button above. In this tab, we assume that we do not know the generating process, and we want to create a statistical model that helps to explain our data.",
            "This is a list of our generated data from the DGP tab.",
            "Here, you get to select the distribution that you think explains the relationship between potential covariates and your generated data. Of course, you know the true distribution from the DGP tab, but you can replicate the real-world and try some other options, too.",
            "Now you can select covariates that you want to be used in the model. Again, you know which covariates you used in the DGP tab, but you can replicate the real-world by choosing different ones, too. You can use the plus or minus buttons to add or remove covariates.",
            "This is the statistical model for our assumed distribution, which tells us how our covariates relate to our outcome variable. Take note of the parameters here, which you will set in the next step.",
            "Now you can select the parameters by hand. Try to get the green density line in the below graph to match with the histogram of the observed data the best you can! Alternatively, look at the log-likelihood plot, and try to set the parameter to where the plot reaches its maximum likelihood. Or, if you'd rather let the math work for you, simply select the Set to MLE button, which will set all your parameters to the values which maximize the log-likelihood functions.",
            "This is a plot which shows a histogram of your observed data, as well as a green density plot of your assumed distribution (i.e. what we would expect to see if your assumed distribution, covariates, and parameters were real). Try to make the green density plot match the blue histogram with the sliders above!",
            "This is the likelihood and log-likelihood function for the data. This comes from the statistical model above, and plots the curve to the right.",
            "This is our log-likelihood plot. It takes the log-likelihood function on the left, and graphically displays this as a curve. Also note the quadratic approximation, which helps us to look at the curvature at the maximum for uncertainty estimates. Notice how when you change the parameter slider, the vertical line will move. Try to choose the parameter value such that this line intersects the curve at its maximum. That will produce the maximum likelihood estimate by hand.",
            "These are the maximum likelihood estimates for our parameters. These are the values which maximize the function above and the curve to the left.",
            "This is the variance-covariance matrix for our maximum likelihood estimates. This tells us how much uncertainty we have around our estimates, as we will see in the next tab.",
            "This plot shows you how your assumed covariate relates to your intermediate parameter, according to the assumed distribution and assumed beta parameters. If you selected more than one covariate, you can change the covariate below the x axis.",
            "This is tutorial mode, a guided walk-through of 2k1 In Silico. If you are already familiar with the app, you can use the exit button above. In this tab, we use the model created in the previous tab to simulate specific quantities that you may be interested in.",
            "These are the maximum likelihood estimates for our parameters, from the previous model tab.",
            "This is the variance-covariance matrix estimates from the previous model tab. These provide us with uncertainty estimates when we run our simulations.",
            "Here, you can choose which quantity of interest you would like to simulate. We have four options (though not all will be possible for most DGPs): Predicted values, which are possible outcome data values; Probability(Y > 1), which is the estimated probability that your outcome data will be greater than 1; Expected Values, which simulated the average of the outcome for the given covariate values; and Simulated Parameters, which simulates the intermediate parameter values, where applicable.",
            "Now you can select the values for the assumed covariates, allowing you to predict outcome values for this specific case.",
            "This is our estimation uncertainty. It describes how we only see a sample of data, and that our MLEs are normally distributed, with some uncertainty around those estimates.",
            "This is our fundamental uncertainty. It shows that even with the covariates we can measure, there is going to be uncertainty because of the real-world covariates that we did not include in our model.",
            "Here is a histogram plot of your chosen quantity of interest.",
            "Here is the functional form plot which shows how your chosen covariate relates to the intermediate parameter. Notice how there is shading for the uncertainty in our estimates.",
            "This is tutorial mode. You should begin tutorial mode when you switch to each new tab. <br> <br> Tutorial mode will introduce you to 2K1, show you the big picture, and simultaneously explain the details of each individual part of the app. <br> <br> Also notice the grey question mark icons at the top menu bar. Click on these when you are confused about what you are doing. These help explain the big picture, detailing each tab in the context of the entire process. If you feel lost or confused, these should be your first stop to not lose the forest for the trees.",
            "This is the tab for the home page. To get back to the home page at any time, simply click on the 2K1 shield. <br> <br> Mastering statistical concepts requires repeated practice, but learning to code in R often diverts attention from the statistics itself. 2K1 simplifies this process, letting students focus on the material without needing coding skills. After lectures and readings, students can use 2K1 to generate data, explore how different choices affect outcomes, estimate models based on the data, and calculate key measures of interest. This approach allows for a deeper understanding of statistics in real-world contexts while keeping both the details and the big picture in view.",
            "The DGP tab provides a peek behind the curtains into how all the data we might want to use comes into being. In the real world, this isn’t possible - usually, we find or collect a set of data, like the number of airstrikes in a conflict or a record of politicians’ votes in Congress, and start from there. It’s really important, though, to try to understand why the distribution of airstrikes (or congressional votes) looks the way it does (which we discuss in the inference tab!). While in real life we usually can’t know for sure how our data comes to look as it does, this tab is a world where we are in full control.",
            "Unlike the DGP tab, which gives you the chance to go behind the scenes and create datasets, this is the tab that mirrors what we usually do with data in the real world: we have some values, and we want to figure out what process most likely created them: performing inference. A process we can’t see is doing the steps in the DGP tab to generate some data points that we do get to see, and we are trying to backtrace and figure out what that generation process is.",
            "In the QOI tab, you can mimic using the hard-earned results of your inferences to estimate values that you actually care about - this is often a key goal in statistics. From your set of real-world data (and remember, in the real world we cannot see the DGP that creates this data, though 2k1 lets us act like we can), you make an educated choice about what the DGP is: what type of model is most likely to have generated the dataset you have received. Then, you can use this model and the real-world data to compute quantities of interest - what are your predicted values, or expected values, based on the data you have and how you think it was formed?",
            "For additional information, we have provided a link to our paper, course materials, documentation, as well as links to helpful resources such as substantive applications, technical lesson plans, and more."
            ),
  highlightClass = rep("introjs-large-highlight", 39)
))

### OPTIONS ########################################

options(warn = -1,
        spinner.color="#9a2b35",
        spinner.size=0.7,
        "launch.browser" = "T")#, shiny.fullstacktrace = T)


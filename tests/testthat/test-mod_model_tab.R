# files.sources = list.files("./R/")
# files.sources = paste0("./R/",files.sources)
# sapply(files.sources, source)

# avoid same seed being used always
rm(.Random.seed, envir=globalenv())

distrID_rand <- sample(unlist(optGroups, use.names=FALSE), 1)
distrID_rand<-"Poisson"
distrConfig <- distrDF[.(distrID_rand)]

assumedDistrID_rand <-  sample(unlist(parser_lst(distrConfig$assumedDistrChoices)),1)
assumedDistrID_rand<-"Poisson (Exp)"
assumedDistrConfig <- distrDF[.(assumedDistrID_rand)]




cat(paste0("\n testing with distrID: ", distrID_rand,
           "\n and assumedDistrID: ", assumedDistrID_rand,
           "\n"
))

outcomeData_file <- read.csv("C:/Users/natra/Documents/Technologies/R/2k1-in-silico/tests/test_samp_vals/outcomeData.csv")
outcomeData <- sample(outcomeData_file[outcomeData_file$distrID == distrID_rand,]$outcomeData, 1)
outcomeData <- as.numeric(strsplit(gsub('[{]|[}]', '', outcomeData), ',')[[1]])
xChoices_file <- read.csv("C:/Users/natra/Documents/Technologies/R/2k1-in-silico/tests/test_samp_vals/xChoices.csv")
xChoices <- sample(xChoices_file[xChoices_file$distrID == distrID_rand,]$xChoices, 1)
assumedXChoice1 <-sample(unlist(xGenerationChoices), 1)
assumedXChoice2 <-sample(unlist(xGenerationChoices), 1)

min_param <- assumedDistrConfig$sliderMin
max_param <- assumedDistrConfig$sliderMax
byHand1 <- round(runif(1, min=min_param, max=max_param),3)
byHand2 <- round(runif(1, min=min_param, max=max_param),3)
byHand3 <- round(runif(1, min=min_param, max=max_param),3)
byHand4 <- round(runif(1, min=min_param, max=max_param),3)
byHand5 <- round(runif(1, min=min_param, max=max_param),3)
byHand6 <- round(runif(1, min=min_param, max=max_param),3)


assumedXChoice1 <-sample(unlist(xGenerationChoices), 1)
assumedXChoice2 <-sample(unlist(xGenerationChoices), 1)
assumedXChoice3 <-sample(unlist(xGenerationChoices), 1)
assumedXChoice4 <-sample(unlist(xGenerationChoices), 1)

numXAssumed <- ifelse((assumedDistrConfig$nCovar > 1),2,1)

simX1 <- (-1)^2*.1
simX2 <- (-2)^2*.1
simX3 <- (-3)^2*.1
simX4 <- (-4)^2*.1

simXVals <- c(1,inputsClean(list(simX1=simX1,
                                 simX2=simX2,
                                 simX3=simX3,
                                 simX4=simX4),
                            "simX", numXAssumed-1))

marginalSelectedLLF <- "X1"

#########################################################
# recreate MLEResult since difficult to save from model tab for testing
#########################################################

assumedXChoices <- inputsClean(list(assumedXChoice1=assumedXChoice1,
                                    assumedXChoice2=assumedXChoice2,
                                    assumedXChoice3=assumedXChoice3,
                                    assumedXChoice4=assumedXChoice4),
                               "assumedXChoice", numXAssumed-1)
assumedXVals <- if(assumedDistrConfig$nCovar > 1){
  xValGenerator(length(outcomeData),
                assumedXChoices)} else {NULL}

mcListLL <- if (assumedDistrConfig$nVar > 1){
  firstParamName <- capitalizeStr(
    substr(assumedDistrConfig$paramTex, 2,
           nchar(assumedDistrConfig$paramTex)))
  if(!is.na(assumedDistrConfig$secondParamTex)){
    secondParamName <- "Gamma"
    c(lapply(0:(numXAssumed-1), function(i){paste0(firstParamName,i)} ),
      secondParamName )
  } else {
    lapply(0:(numXAssumed-1), function(i){paste0(firstParamName,i)} )
  }
}

marginalSelectedLL <- if(assumedDistrConfig$nCovar>1){unlist(sample(mcListLL, 1))}else("Beta0")
tmp <- which(mcListLL == marginalSelectedLL)
margNumLL <- if(length(tmp) == 0){1} else{tmp}

cat("marginalSelectedLL: ",marginalSelectedLL)
# MLEResult <-  likelihoodEstimateFun(
#   chartDomain = parser(assumedDistrConfig$chartDomain)(numXAssumed + assumedDistrConfig$nNonXParams),
#   likelihoodFun = parser(assumedDistrConfig$likelihoodFun),
#   margNum = margNumLL,
#   outcome = outcomeData,
#   xVals = assumedXVals,
#   optimMethod = assumedDistrConfig$optimMethod,
#   nParams = numXAssumed + assumedDistrConfig$nNonXParams)
#
# cat("\n MLEResult paramHat: ",MLEResult$paramHat)
# cat("\n MLEResult paramVCov: ",MLEResult$paramVCov)

###############################################################
## RUN TESTS
###############################################################

test_that("numXAssumed not na", {
  testServer(mod_model_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       outcomeData=reactive(outcomeData),
                       xChoices=reactive(xChoices)),
             {
            session$setInputs(
              assumedDistrID = assumedDistrID_rand,
              )
            cat("numXAssumed: ")
            cat(numXAssumed())
            expect_true(!is.na(numXAssumed() &
                                 !is.null(numXAssumed())))
          })
})

test_that("assumedDistrConfig has content", {
  testServer(mod_model_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       outcomeData=reactive(outcomeData),
                       xChoices=reactive(xChoices)),
             {
             session$setInputs(
               assumedDistrID = assumedDistrID_rand,
             )
             expect_true(length(unlist(assumedDistrConfig()))>1)
           })
})


test_that("mcListLL not na", {
  testServer(mod_model_tab_server,
                args=list(distrConfig=reactive(distrConfig),
                   outcomeData=reactive(outcomeData),
                   xChoices=reactive(xChoices)),
             {
             session$setInputs(
               assumedDistrID = assumedDistrID_rand,
               assumedXChoice1 = assumedXChoice1,
               assumedXChoice2 = assumedXChoice2
             )
             expect_true(!any(is.na(mcListLL())))
             assign("marginalSelectedLL",mcListLL()[[1]],
                    envir=.GlobalEnv)
             cat("Marginal Selected LL:")
             cat(marginalSelectedLL)
             cat(" ")
           })
})


test_that("margNumLL not na", {
  testServer(mod_model_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       outcomeData=reactive(outcomeData),
                       xChoices=reactive(xChoices)),
             {
             session$setInputs(
               assumedDistrID = assumedDistrID_rand,
               assumedXChoice1 = assumedXChoice1,
               assumedXChoice2 = assumedXChoice2,
               marginalSelectedLL = marginalSelectedLL,
               resetByHand = 2
             )
             cat("margNumLL: ")
             cat(margNumLL())
             expect_true(!is.na(margNumLL()))
           })
})


test_that("assumedXVals not na", {
  testServer(mod_model_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       outcomeData=reactive(outcomeData),
                       xChoices=reactive(xChoices)),
             {
             session$setInputs(
               assumedDistrID = assumedDistrID_rand,
               assumedXChoice1 = assumedXChoice1,
               assumedXChoice2 = assumedXChoice2,
               marginalSelectedLL = marginalSelectedLL,
               resetByHand = 2
             )
             cat("assumedXVals: ")
             cat(assumedXVals())
             expect_true(!any(is.na(assumedXVals())))
           })
})


test_that("byHandParams not na", {
  testServer(mod_model_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       outcomeData=reactive(outcomeData),
                       xChoices=reactive(xChoices)),
             {
             session$setInputs(
               assumedDistrID = assumedDistrID_rand,
               assumedXChoice1 = assumedXChoice1,
               assumedXChoice2 = assumedXChoice2,
               marginalSelectedLL = marginalSelectedLL,
               byHand1 = byHand1,
               byHand2 = byHand2,
               byHand3 = byHand3,
               byHand4 = byHand4,
               byHand5 = byHand5,
               byHand6 = byHand6,
               resetByHand = 2
             )
             cat("byHandParams: ")
             cat(byHandParams())
             expect_true(!any(is.na(byHandParams())))
           })
})


test_that("MLEResult not na", {
  testServer(mod_model_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       outcomeData=reactive(outcomeData),
                       xChoices=reactive(xChoices)),
             {
             session$setInputs(
               assumedDistrID = assumedDistrID_rand,
               assumedXChoice1 = assumedXChoice1,
               assumedXChoice2 = assumedXChoice2,
               marginalSelectedLL = marginalSelectedLL,
               byHand1 = byHand1,
               byHand2 = byHand2,
               byHand3 = byHand3,
               byHand4 = byHand4,
               byHand5 = byHand5,
               byHand6 = byHand6,
               resetByHand = 2
             )

             expect_true(!any(is.na(MLEResult())))
           })
})


test_that("testMLEbyHand not na", {
  testServer(mod_model_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       outcomeData=reactive(outcomeData),
                       xChoices=reactive(xChoices)),
             {
               session$setInputs(
                 assumedDistrID = assumedDistrID_rand,
                 assumedXChoice1 = assumedXChoice1,
                 assumedXChoice2 = assumedXChoice2,
                 marginalSelectedLL = marginalSelectedLL,
                 byHand1 = byHand1,
                 byHand2 = byHand2,
                 byHand3 = byHand3,
                 byHand4 = byHand4,
                 byHand5 = byHand5,
                 byHand6 = byHand6,
                 resetByHand = 2
               )
               cat("testMLEbyHand: ")
               cat(testMLEbyHand()[[1]],"\n")
               cat(testMLEbyHand()[[2]],"\n")
               cat(testMLEbyHand()[[3]])
               expect_true(!any(is.na(testMLEbyHand())))
             })
})

test_xAxis <- seq(0,20,1)
test_hprobs <- c(2.236317e-05, 0.000239467, 0.001282118, 0.004576346, 0.01225099, 0.02623694, 0.04682462, 0.07162892, 0.09587616, 0.1140723, 0.1221497, 0.1189083, 0.1061068, 0.0874001, 0.06684918, 0.04772183, 0.03193812, 0.02011743, 0.01196774, 0.006744829, 0.003611213)
test_hypothesized <- tibble::tibble(drawVal = test_xAxis,hprobs = test_hprobs)
data <- outcomeData

test_observed <- data.frame(data = test_xAxis) %>% left_join(
  as.data.frame(table(data)/length(data)) %>%
    dplyr::mutate(data = as.integer(as.character(data))),
  by = "data")
colnames(test_observed) <- c("drawVal", "oprobs")
test_observed$oprobs[is.na(test_observed$oprobs)] <- 0
test_histData <- left_join(test_observed, test_hypothesized, by = "drawVal")


p <- ggplot2::ggplot(test_histData)  +
  ggplot2::geom_bar(mapping = aes(x = drawVal, y = oprobs),
           stat="identity", alpha = .25, position = "identity",
           fill = baseColor,
           color = baseColor)

for(j in 1:length(test_hprobs)){
  p <- p + eval(parse(text = paste0(
    "geom_segment(aes(x = -.5+",test_xAxis[j],", xend = .5+",test_xAxis[j],
    ", y = hprobs[",j,"], yend = hprobs[",j,"]),size = 1.2, color = baseColor2)"
  ))) #if GGplot wasn't so goddamn 'clever'....
}

yRangeMax <- if(nrow(test_histData) < 4){max(1, max(test_histData$oprobs) + .2)} else {.1}

p <- p +
  labs(x = "y", y = "Observed Probability") +
  ylim(0,yRangeMax) +
  theme(legend.position = "none",
        plot.caption = element_text(
          size=12, margin = ggplot2::margin(t = 10), hjust = 0.5),
        axis.text.x = element_blank(),#element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16,
                                    margin = unit(c(4, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 16,
                                    margin = unit(c(4, 4, 4, 4), "mm"), color = baseColor)
  )
p




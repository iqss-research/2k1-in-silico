# files.sources = list.files("./R/")
# files.sources = paste0("./R/",files.sources)
# sapply(files.sources, source)

library(dplyr)

# avoid same seed being used always
rm(.Random.seed, envir=globalenv())

distrID_rand <- sample(unlist(optGroups, use.names=FALSE), 1)
distrID_rand<-"Normal (X)"
distrConfig <- distrDF[.(distrID_rand)]

assumedDistrID_rand <-  sample(unlist(parser_lst(distrConfig$assumedDistrChoices)),1)
assumedDistrID_rand<-"Normal (X)"
assumedDistrConfig <- distrDF[.(assumedDistrID_rand)]


cat(paste0("\n testing with distrID: ", distrID_rand,
           "\n and assumedDistrID: ", assumedDistrID_rand,
           "\n"
))

outcomeData_file <- read.csv("C:/Users/natra/Documents/Technologies/R/2k1-in-silico/tests/test_samp_vals/outcomeData.csv")
outcomeData <- sample(outcomeData_file[outcomeData_file$distrID == distrID_rand,]$outcomeData, 1)
outcomeData <- as.numeric(strsplit(gsub('[{]|[}]', '', outcomeData), ',')[[1]])

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

MLEResult <-  likelihoodEstimateFun(
    chartDomain = parser(assumedDistrConfig$chartDomain)(numXAssumed + assumedDistrConfig$nNonXParams),
    likelihoodFun = parser(assumedDistrConfig$likelihoodFun),
    margNum = margNumLL,
    outcome = outcomeData,
    xVals = assumedXVals,
    optimMethod = assumedDistrConfig$optimMethod,
    nParams = numXAssumed + assumedDistrConfig$nNonXParams)

cat("\n MLEResult paramHat: ",MLEResult$paramHat)
cat("\n MLEResult paramVCov: ",MLEResult$paramVCov)

#######################################################
# PERFORM TESTS
#######################################################


test_that("paramTilde not na", {
  testServer(mod_qoi_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       assumedDistrConfig=reactive(assumedDistrConfig),
                       MLEResult=reactive(MLEResult),
                       numXAssumed=reactive(numXAssumed),
                       assumedXVals=reactive(assumedXVals)),
             {
               session$setInputs(
                 assumedDistrID = assumedDistrID_rand,
               )
               cat("paramTilde: ")
               cat(paramTilde()[1:10,])
               expect_true(!any(is.na(paramTilde()) &
                                    !any(is.null(paramTilde()))))
             })})


test_paramTilde <- read.csv("tests/test_samp_vals/test_paramTilde.csv",
                            nrows=1,
                            header = FALSE,
                            row.names=NULL) %>%
                     unlist() %>%
                     as.vector() %>%
                     matrix(ncol=length(MLEResult$paramHat),
                               byrow=TRUE)

intrTildeCreator <- function(paramTilde, transformFun, xVals = c(1)){

  intrTilde <- sapply(1:nrow(paramTilde),
                      function(a){transformFun(paramTilde[a,], xVals = c(xVals), DGP = F)})
  intrTilde <- if(!is.null(dim(intrTilde))){
    intrTilde %>%  t()
  } else {intrTilde}
}

test_intrTilde<- intrTildeCreator(test_paramTilde,parser(assumedDistrConfig$transformFun), simXVals)

test_expVals <- expValCreator(test_intrTilde, parser(assumedDistrConfig$drawFun))


test_that("intrTilde not na", {
  testServer(mod_qoi_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       assumedDistrConfig=reactive(assumedDistrConfig),
                       MLEResult=reactive(MLEResult),
                       numXAssumed=reactive(numXAssumed),
                       assumedXVals=reactive(assumedXVals)),
             {
               session$setInputs(
                 assumedDistrID = assumedDistrID_rand,
                 simX1=simX1,
                 simX2=simX2,
                 simX3=simX3,
                 simX4=simX4
               )
               cat("intrTilde: ")
               cat("\n",dim(intrTilde()))
               cat("\n", intrTilde()[1:5,])
               expect_true(!any(is.na(intrTilde()) &
                                  !any(is.null(intrTilde()))))
             })})


test_that("yTilde not na", {
  testServer(mod_qoi_tab_server,
             args=list(distrConfig=reactive(distrConfig),
                       assumedDistrConfig=reactive(assumedDistrConfig),
                       MLEResult=reactive(MLEResult),
                       numXAssumed=reactive(numXAssumed),
                       assumedXVals=reactive(assumedXVals)),
             {
               session$setInputs(
                 assumedDistrID = assumedDistrID_rand,
                 simX1=simX1,
                 simX2=simX2,
                 simX3=simX3,
                 simX4=simX4
               )
               cat("yTilde: ")
               cat("\n",length(yTilde()))
               cat("\n", yTilde()[1:5])
               expect_true(!any(is.na(yTilde()) &
                                  !any(is.null(yTilde()))))
             })})


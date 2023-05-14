#files.sources = list.files("./R/")
#files.sources = paste0("./R/",files.sources)
#sapply(files.sources, source)

# avoid same seed being used always
rm(.Random.seed, envir=globalenv())


#distrID_rand <- sample(unlist(optGroups,use.names=FALSE), 1)
distrID_rand <- "Neg Binomial (X)"
#distrID_rand <- "Stylized Normal (X)"
nObs <- 20
min_param <- distrDF$sliderMin
max_param <- distrDF$sliderMax
param1 <- round(runif(1, min=min_param, max=max_param),3)
param2 <- round(runif(1, min=min_param, max=max_param),3)
param3 <- round(runif(1, min=min_param, max=max_param),3)
param4 <- 1.1 # for Neg Binom signma param

xChoice1 <- sample(unlist(defaultXChoices), 1)
xChoice2 <- sample(unlist(defaultXChoices), 1)
xChoice3 <- sample(unlist(defaultXChoices), 1)

cat(paste0("\n testing with distrID: ", distrID_rand,
             "\n and nObs: ",nObs,
             "\n and params: ",param1,", ",param2,", ",param3,
             "\n and xChoices: ",xChoice1,", ",xChoice2,",",xChoice3))

test_that("numX not na or null",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,)
    expect_true(!is.na(numX() & !is.null(numX())))
    cat("numX:")
    cat(numX())
  })
})

test_that("numX is integer gt 0",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs)
    expect_gte(numX(), 1)
  })
})

test_that("probParams not na",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3)
    expect_true(!any(is.na(probParams())))
    cat("probParams: ")
    cat(probParams())
  })
})

test_that("xChoices not na",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3
                      )
    expect_true(!any(is.na(xChoices())))
      cat("xChoices: ")
      cat(xChoices())
      write.table(data.table(distrID_rand,
                             paste(unlist(xChoices()),
                                   collapse=",")),
                  file="C:/Users/natra/Documents/Technologies/R/2k1-in-silico/tests/test_samp_vals/xChoices.csv",
                  append=TRUE,
                  sep=",",
                  row.names=FALSE,
                  col.names=FALSE)

  })
})

test_that("xVals not na",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3
                      )
    expect_true(!any(is.na(xVals())))
    cat("xVals: ")
    cat(xVals())
  })
})


test_that("paramsTransformed not na",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3
                      )
    expect_true(!any(is.na(paramsTransformed())))
    cat("paramsTransformed: ")
    cat(paramsTransformed())
  })
})

test_that("outcomeData not na",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3
                      )
    expect_true(!any(is.na(outcomeData())))
    cat("Outcome Data: ")
    cat(outcomeData())
    write.table(data.table(distrID_rand,
                           paste(unlist(outcomeData()),
                                        collapse=",")),
        file="C:/Users/natra/Documents/Technologies/R/2k1-in-silico/tests/test_samp_vals/outcomeData.csv",
        append=TRUE,
        sep=",",
        row.names=FALSE,
        col.names=FALSE)
  })
})

test_that("outcomeData longer than 1",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3
                      )
    expect_gt(length(outcomeData()), 1)
  })
})


test_that("distPlot plot accessible without error",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3
                      )
    expect_equal(1,1)
    output$distPlot
  })
})

test_that("probHistPlot plot accessible without error",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3)
    expect_equal(1,1)
    output$probHistPlot
  })
})

test_that("ordinalPlot plot accessible without error",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3
                      )
    expect_equal(1,1)
    output$ordinalPlot
  })
})

test_that("functionalFormPlot plot accessible without error",{
  testServer(mod_dgp_tab_server, {

    # Set and test an input
    session$setInputs(distrID = distrID_rand,
                      nObs = nObs,
                      addXVar=TRUE,
                      param1 = param1,
                      param2 = param2,
                      param3 = param3,
                      xChoice1 = xChoice1,
                      xChoice2 = xChoice2,
                      xChoice3 = xChoice3
                      )
    expect_equal(1,1)
    output$functionalFormPlot
  })
})


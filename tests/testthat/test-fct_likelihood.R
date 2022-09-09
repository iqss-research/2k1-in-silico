test_that("Basic MLE functionality", {

  set.seed(2001)
  testData <- styNormDraws(.5,800)
  testXVals <- xValGenerator(nRow = 800)

  result <- likelihoodEstimateFun(
    chartDomain = styNormChartDomain(1),
    likelihoodFun = styNormLikelihoodFun,
    margNum = NULL, outcome =testData,
    nParams = 1, xVals =  testXVals, optimMethod = "L-BFGS-B"
  )

  expect_lt(abs(result$paramHat- .5), .05)

})


test_that("Nonsingular Hessian should return coeffs but no SE", {

  set.seed(2001)
  testParams <- c(.2,.5,.2)
  testXVals <- xValGenerator(nRow = 800, c("Normal A", "Normal A"))
  transfParams <- sapply(
    1:800,
    function(i){styNormXParamTransform(testParams, testXVals[i,], DGP = T)})
  testData <- styNormXDraws(params = transfParams, nObs = 800)


  result <- likelihoodEstimateFun(
    chartDomain = styNormXChartDomain(3),
    likelihoodFun = styNormXLikelihoodFun,
    margNum = 1, outcome =testData,
    nParams = 3, xVals =  testXVals, optimMethod = "L-BFGS-B"
  )

  expect_lt(abs(result$paramHat[[1]] - testParams[[1]]), .05)
  expect_lt(abs(result$paramHat[[2]]  + result$paramHat[[3]]-
                  testParams[[2]] - testParams[[3]]), .05)
  expect_true(is.na(result$paramSE))

})


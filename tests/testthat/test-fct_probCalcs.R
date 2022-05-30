test_that("inputsClean removes NA params", {
  inputTest <- list(
    param1 = 1,
    param2 = 2,
    param3 = NA,
    param4 = 2
  )
  numTests <- 5
  numX <- 2
  nNonXParams <- 1

  expect_equal(
    inputsClean(inputTest, "param", numTests = numTests, numOuts = numX + nNonXParams), c(1,2,2))
})

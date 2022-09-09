test_that("Stylized Normal PDF tests", {
  testthat::expect_lt(styNormPDF(drawVal = 10, param = 0),1/100)
  testthat::expect_equal(styNormParamTransform(1.5, c()), 1.5)
})



test_that("binary plotting works for ordered logit", {

  testConfig <- distrTestSetup(
    "Ordered Logit (X)", paramsInput = c(.5,1,0,0, 1),
    xChoiceInput = c("Normal A", "Normal A", "Normal A"))
  orderedProbitXPlotDistr(
    testConfig$paramsTransformed %>%  as.matrix(),
    parser(testConfig$distrConfig$analyticDomain),
    parser(testConfig$distrConfig$analyticRange)
  )

  expect_true(TRUE)

})

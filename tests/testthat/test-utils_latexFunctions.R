test_that("LaTeX rendering of output works", {
  expect_equal(vCovLatex("\\mu", c(1)), tags$p("\\( \\hat{V}(\\hat{\\mu}) = 1 \\)"))
})

set.seed(2001)
distrDF <- readxl::read_excel(app_sys("DistrNames.xlsx"),1)
nObs <- 50

distrTestSetup <- function(distrID, paramsInput = c(1,3,4,2),
                           xChoiceInput = c("Normal A", "Poisson B")){
  # distrID <- "Ordered Logit (X)"
  # paramsInput <- c(1,-2,1.25,.5, .5)
  # xChoiceInput <- c("Normal A", "Normal B", "Normal C")

  distrConfig <- distrDF %>%  dplyr::filter(distrList == distrID)
  stopifnot(assertthat::are_equal(
    length(paramsInput),
    distrConfig$nNonXParams + length(xChoiceInput) + 1))

  xVals <-xValGenerator(nRow = nObs, type = xChoiceInput)
  paramsTransformed <- sapply(
    1:nObs,
    function(i){(parser(distrConfig$transformFun))(paramsInput, xVals[i,], DGP = T)})

  ret <- list(
    distrID = distrID,
    distrConfig = distrConfig,
    xVals = xVals,
    probParams = paramsInput,
    paramsTransformed = paramsTransformed
  )

}

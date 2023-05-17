#' probCalcs
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# numTests needs to be at least as large as max numX() + nNonXParams()
inputsClean <- function(input, inputName, numOuts, numTests = 6){
  vec <- sapply(1:numTests, function(n){
    t <- input[[paste0(inputName,n)]]
    if(!isnothing(t)){t} else {NA}
  })
  vec[!is.na(vec)][1:numOuts] %>%  unlist()
}



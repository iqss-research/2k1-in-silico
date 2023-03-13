#' probCalcs
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

inputsClean <- function(input, inputName, numOuts, numTests = 5){
  vec <- sapply(1:numTests, function(n){
    t <- input[[paste0(inputName,n)]]
    if(!isnothing(t)){t} else {NA}
  })
  vec[!is.na(vec)][1:numOuts] %>%  unlist()
}



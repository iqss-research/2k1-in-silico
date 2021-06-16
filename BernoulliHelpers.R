bernDraws <- function(piParam, nTrials){
  
  random <- runif(nTrials) # n i.i.d. uniform draws
  outcome <- ifelse(random <= piParam, 1, 0) # how many < pi
  
  return(outcome)
}

# Function mapping parameters pi to likelihood
bernLikelihoodFun <- function(testParam, nSuccesses, nObs){log((testParam^(nSuccesses))*((1-testParam)^(nObs - nSuccesses)))}

bernMLE <- function(outcome, intervals = 20){
  
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  
  testPiParam <- (1:intervals)/intervals
  probOutcomeGivenPi <- bernLikelihoodFun(testPiParam, nSuccesses, nObs)
  
  return <- data.frame(Pi = testPiParam, Likelihood = probOutcomeGivenPi)
  
}


bernDataPrintHelper <- function(header, bernData, printLength = 25){
  
  message(header, appendLF = TRUE)
  lapply(bernData[1:min(printLength, length(bernData))], function(a){
    message(a)
    Sys.sleep(0.00001)
  })
  if(length(bernData) > printLength){message("...")}
  
}


bernPlot <- function(outcome){
  
  likelihoodDB <- bernMLE(outcome = outcome, intervals = 100) %>% rename(`Log Likelihood` = Likelihood)
  
  nObs <- length(outcome)
  nSuccesses <- sum(outcome)
  
  chartDomain <- 1:100/100
  
  likelihoodDB <- likelihoodDB %>%  left_join(
    quadraticLikelihoodApprox(
      likelihoodFun = bernLikelihoodFun, chartDomain = chartDomain, testParams = .5, nSuccesses = nSuccesses, nObs = nObs
    ), by = c("Pi" = "param") ) %>% 
    rename(`Quadratic Approx` = QuadraticApprox)
  
  ggplot() + geom_line(data = likelihoodDB, mapping =  aes(x = Pi, y = `Log Likelihood`), color = "steelblue") + 
    geom_line(data = likelihoodDB, mapping =  aes(x = Pi, y = `Quadratic Approx`), color = "firebrick4") +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    )
  
}



bernMarkdown <- function(type){
  
  if(type == "distr"){
    
    rmarkdown::render("markdown\\BernDistr.Rmd", quiet = TRUE)
    withMathJax(includeMarkdown("markdown\\BernDistr.md"))
    
  }
  else if(type == "statModel"){
    
    rmarkdown::render("markdown\\BernModel.Rmd", quiet = TRUE)
    withMathJax(includeMarkdown("markdown\\BernModel.md"))
    
  } else if(type == "likelihood"){
    
    rmarkdown::render("markdown\\BernLikelihood.Rmd", quiet = TRUE)
    withMathJax(includeMarkdown("markdown\\BernLikelihood.md"))
    
  } else stop("Unknown Markdown!")
  
  
}

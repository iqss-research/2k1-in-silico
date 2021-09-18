
############################################################
# Generic Helpers
############################################################



in_silence <- function(...)
{
  mc <- match.call()[-1]
  a <- capture.output(
    tryCatch(
      suppressMessages(suppressWarnings(
        eval(as.list(mc)[[1]])
      )), error = function(e) ""))
}

# send a string f that parses to a function. Use ? instead of i. 
# creates this object in the specified environment. Returns nothing. Use CAREFULLY for side effects. 
# sorry this is terrible
listParser <- function(num, funStr, envToUse){
  
  for(i in 1:num){
    eval(parse(text = gsub("\\?", i, funStr)), envir = envToUse )
    
  }
  
  return(NULL)
}

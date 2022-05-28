

############################################################
# Generic Helpers
############################################################


capitalizeStr <- function(str){

  firstChar <- substr(str,1,1)
  chartr(firstChar, toupper(firstChar), str)

}


isnothing = function(x) {
  if(!is.null(x)){is.na(x)|is.nan(x)} else (is.null(x))
}

in_silence <- function(...)
{
  mc <- match.call()[-1]
  a <- capture.output(
    tryCatch(
      suppressMessages(suppressWarnings(
        eval(as.list(mc)[[1]])
      )), error = function(e) ""))
}



parser <- function(a){eval(parse(text = a))}

# send a string f that parses to a function. Use ? instead of i.
# creates this object in the specified environment. Returns nothing. Use CAREFULLY for side effects.
# sorry this is terrible
listParser <- function(num, funStr, envToUse){

  for(i in 1:num){
    eval(parse(text = gsub("\\?", i, funStr)), envir = envToUse )

  }

  return(NULL)
}




sciNotTex <- function(a){
  tmp <- sprintf("%.1e", a)
  exp <- str_sub(tmp, -3, -1)
  base <- str_sub(tmp,1,3)
  paste0("{ \\small",base,"\\text{e}^{",exp," }}")}

roundOrShrink <- function(a){
  if(abs(round(a,2) - 0) > 1e-5 || a == 0){return(round(a,2))} else{sciNotTex(a)}}

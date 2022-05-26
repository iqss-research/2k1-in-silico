#' launch 2k1 in silico
#' 
#' @export
run2k1 <- function()  {

  source("R/preamble.R")
  a<- shiny::shinyApp(ui = ui2k1, server = server,
           onStart = function(){
             #HACK
             oldw <<- getOption("warn")
             options(warn = -1)#, shiny.fullstacktrace = T)
             onStop(function(){
               options(warn = oldw)
               
             })
             
           })
  
  runApp(a, launch.browser = T)
  
}
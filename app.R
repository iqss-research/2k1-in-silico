sapply(list.files("R/"), function(a)(source(paste0("R/", a))))


shinyApp(ui = ui2k1(), server = server,
         onStart = function(){
           oldw <<- getOption("warn")
           options(warn = -1)#, shiny.fullstacktrace = T)
           onStop(function(){
             options(warn = oldw)
             
           })
           
         })



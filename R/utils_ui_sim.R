## Utils functions for Sim UI tab


############################################################
# Sim UI
############################################################


# function making sliders for the sim pages
simMultiSliderFunction <- function(ns, numSliders){

  if(numSliders == 0){""} else{

    column(12,
           lapply(1:numSliders, function(i){
             xSubscript <- ifelse(numSliders ==1, "<sub>c</sub>", paste0("<sub>c,",i,"</sub>"))
             column(12,div(
               div(HTML(paste0("<p style='color:#ff0000'><b>X",xSubscript,"</b></p>")),
                   style = "float:left; padding-right:10px"),
               div(
                 id = paste0("simSliderDiv",i),
                 sliderInput(
                   ns(paste0("simX",i)),
                   NULL,
                   min = -2,
                   max = 2,
                   value = (-i)^2*.1,
                   step = .1,
                   width = paramSliderWidth), style = "float:left;")))
           }), style = "margin-left:0px"
    )
  }

}

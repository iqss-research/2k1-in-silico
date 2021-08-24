

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

############################################################
# Printing and other UI
############################################################



decPrintHelper <- function(header, data, printLength){

  if(length(data) > printLength){truncData <- data[1:printLength]}
  else{truncData <- data}
  charData <- lapply(truncData, function(s){sprintf("%0.1f",s)}) %>%  unlist()
  
  printstr <- paste(c(charData), collapse = ", ")
  printstr <- paste(header, printstr, sep = "")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
}


intPrintHelper <- function(header, data, printLength){

  printstr <- paste(c(header, data), sep = " ")
  if(length(data) > printLength){printstr <- paste0(printstr, " ...")}
  
  printstr
}


marginalSelectInput <- function(num, pageNum, choicesInput, session = session){
  
  if(num ==1) {
    shinyjs::hide("marginalSelector")
    ret <- tags$script(paste0("Shiny.setInputValue('marginalSelected'",pageNum, ", 1)")) ### NOT WORKING
    
    } 
  else{
    ret <- selectInput(
      inputId = paste0("marginalSelected",pageNum),
      label = "Choose parameter of profile likelihood to view",
      choices = choicesInput, selected = choicesInput[1] )
  }
  
  ret
}


# function making sliders for the sim pages
simMultiSliderFunction <- function(numSliders){
  
  if(numSliders == 0){""} else{
  
  lapply(1:numSliders, function(i){
    sliderInput(
      paste0("simX",i),
      div(HTML(paste0("<p style='color:#ff0000;'><b>Choose X<sub>", i,"</sub></b></p>"))),
      min = -2,
      max = 2,
      value = (-i)^2*.1,
      step = .1)
  })}
  
}


############################################################
# Plotter
############################################################


continuousDistrPlotter <- function(distrDF, paramVal, paramTex,
                                   annotate = TRUE,
                                   annotationX = NULL,
                                   arrow = TRUE,
                                   roundDigits = 1,
                                   discreteOutput = FALSE,
                                   plotColor = "steelblue"){
    
  if(is.null(annotationX)){annotationX <- mean(distrDF$drawVal)}
  
  paramVal <- as.numeric(paramVal)
  annotationX <- as.numeric(annotationX)
  
  p <- ggplot(distrDF, aes(x = drawVal, y = prob)) + geom_line(color = plotColor , size = 1) +
    labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
    xlim(min(distrDF$drawVal),max(distrDF$drawVal)) +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5))
  

  if(annotate){p <- p +
    annotate("text", x = annotationX, y = quantile(distrDF$prob,.25, na.rm = TRUE),
                             label  = parse(
                               text=TeX(paste0("$",paramTex,"$","=",round(paramVal, roundDigits)), output = "character")),
                             parse = TRUE, color = plotColor)}
  if(arrow){p <- p +
      annotate("segment", x = annotationX, y = quantile(distrDF$prob,.15, na.rm = TRUE), xend = annotationX,
               yend = 0, arrow = arrow(length = unit(0.2, "cm")), color = plotColor)}
  
  if(discreteOutput){p <- p + geom_point(color = plotColor,  size = 3, shape = "square")}
    

  return(p)
  
  
}


binaryDistrPlotter <- function(distrDF, paramVal, paramTex,
                               roundDigits = 1,
                               plotColor1 = "#56B4E9",
                               plotColor2 = "#E69F00"){
  
  
  ggplot(distrDF, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity") +
    scale_fill_manual(values=c(plotColor1, plotColor2)) +
    labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
    theme_minimal() +
    ylim(0,1.2) +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotate("text", x = 0.75, y = 1.1,
                 label  = parse(
                   text=TeX(paste0("$",paramTex,"$","=",round(paramVal, roundDigits)), output = "character")),
                 parse = TRUE, color = plotColor1, size = 6)
  
  
  
}

### takes a vector

histogramMaker <- function(data, title, greaterThan = 999, annotate = F){
  
  histData <- data.frame(value = data)   
  dataMean <- mean(data, na.rm = TRUE)
  dataSD <- sd(data, na.rm = TRUE)
  
  
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  nBins <- min(40, length(unique(histData$value)))
  
  histData <- histData %>%  mutate(grtFlag = (value > greaterThan)) %>%  group_by(grtFlag)
  
  p <- ggplot(histData) + 
    aes(x = value, fill = grtFlag) +
    geom_histogram(
      aes(y= ..count../sum(..count..)), bins = nBins,color = "black", alpha = 0.5, position = "identity") +
    scale_y_continuous(labels = scaleFUN, breaks = seq(0, 100, 10))  + 
    scale_fill_manual(values = c("steelblue","firebrick")) +
    theme_minimal()+
    labs(x = TeX(title)) +
    ylab(element_blank()) +
    labs(title = "", caption = "") +
    theme(legend.position = "none",
          plot.title = element_text(size=12, hjust  = .5, margin = ggplot2::margin(b = 10)),
          plot.caption = element_text(size=7 , margin = ggplot2::margin(t = 10)),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(margin = ggplot2::margin(t = 6)))  
  
  if(annotate){p <- p +
    annotate("text", x = dataMean, y = Inf, vjust = 1, hjust = "left",
             label  = paste0("Mean: ", round(dataMean,1 ),"; SE:", round(dataSD,1 )), color = "black") + 
    annotate("segment", x = dataMean, y = Inf, xend = dataMean, yend = 0, color = "black")}
  
  
  return(p)
  
}



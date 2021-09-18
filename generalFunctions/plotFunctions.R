
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
    annotate("text", x = annotationX, y = .15*max(distrDF$prob,na.rm = TRUE),
             label  = parse(
               text=TeX(paste0("$",paramTex,"$","=",round(paramVal, roundDigits)), output = "character")),
             parse = TRUE, color = plotColor)}
  if(arrow){p <- p +
    annotate("segment", x = annotationX, y = .1*max(distrDF$prob,na.rm = TRUE), xend = annotationX,
             yend = 0, arrow = arrow(length = unit(0.2, "cm")), color = plotColor)}
  
  if(discreteOutput){p <- p + geom_point(color = plotColor,  size = 3, shape = "square")}
  
  
  return(p)
  
  
}


binaryDistrPlotter <- function(distrDF, paramVal, paramTex,
                               roundDigits = 1,
                               plotColor1 = "steelblue",
                               plotColor2 = "steelblue"){
  
  
  ggplot(distrDF, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity", alpha = .5, color = "steelblue") +
    scale_fill_manual(values=c(plotColor1, plotColor2)) +
    labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
    theme_minimal() +
    ylim(0,max(1, distrDF$prob[1] + .2)) +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotate("text", x = 0.75, y = max(distrDF$prob[1]) + .1,
                 label  = parse(
                   text=TeX(paste0("$",paramTex,"$","=",round(paramVal, roundDigits)), output = "character")),
                 parse = TRUE, color = "black", size = 6, fontface = "bold")
  
  
  
}

### takes a vector

histogramMaker <- function(data, title = "", greaterThan = 999, annotate = F, captionText = NULL, ci = NULL, border = T){
  errMessage <- "No data received. Please refresh or change incorrect parameters and try again."
  if(!is.numeric(data) ||! is.null(ncol(data))){
    return(ggplot() + annotate("text", x = 4, y = 1, size=4, label = paste(errMessage, collapse = " ")) + theme_void())}
  
  bordColor <- "steelblue" #if(border){"black"} else{"steelblue"}
  
  histData <- data.frame(value = data)   
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  # make sure bins include 1
  nBins <- min(40, length(unique(histData$value)))
  breaks <- unique(round(pretty.default(data, nBins),2))
  tmpVar <- 0
  while(length(breaks) != 0 && length(which(breaks==1)) ==0 && (max(breaks) > greaterThan)) {
    tmpVar <- tmpVar+1
    breaks <- breaks + (tmpVar*(-1)^(tmpVar-1)/100)
  }
  
  histData <- histData %>%  mutate(grtFlag = (value > greaterThan)) %>%  group_by(grtFlag)
  if(length(breaks) == 1) {breaks <- NULL}
  
  p <- ggplot(histData) + 
    aes(x = value, fill = grtFlag) +
    geom_histogram(
      aes(y=100*..count../sum(..count..)), breaks = breaks,bins = 30, alpha = 0.5,color = bordColor, position = "identity") +
    scale_y_continuous(labels = scaleFUN, breaks = seq(0, 100, 10))  + 
    scale_fill_manual(values = c("steelblue","firebrick")) +
    theme_minimal()+
    labs(x = TeX(title)) +
    ylab(element_blank()) +
    labs(title = "", caption = "") +
    theme(legend.position = "none",
          plot.caption = element_text(size=12, margin = ggplot2::margin(t = 10), hjust = 0.5),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size=12, margin = ggplot2::margin(t = 6)))  
  
  
  dataMean <- mean(data, na.rm = TRUE)
  dataMin <- min(data, na.rm = TRUE)
  dataMax <- max(data, na.rm = TRUE)
  dataRange <- abs(dataMin- dataMax)
  dataSD <- sd(data, na.rm = TRUE)
  # TODO: harmonize
  ydata <- data %>%
    as.data.frame() %>%  mutate(bin = cut(data, breaks, right = F)) %>%  count(bin) %>% 
    mutate(percent = 100*n/sum(n, na.rm = T))
  yMax <- max(ydata$percent, na.rm = T)
  
  
  if(annotate){
    
    
    p <- p + annotate(
      "text", x = dataMean, y = .1*yMax, vjust = 1, hjust = "center",
      label  = paste0(round(dataMean,2),"(", round(dataSD,2 ),")"), color = "firebrick") + 
      annotate("polygon",
               x = c(dataMean, dataMean + .025*dataRange, dataMean - .025*dataRange),
               y = c(0,.05*yMax,.05*yMax), fill = "firebrick", alpha = .5)
  }
  
  if(!is.null(captionText)){
    p <- p + labs(caption = captionText)
  }
  
  if(!is.null(ci)){
    
    p <- p +
      annotate("segment", x = ci[1], xend = ci[2], y = .05*yMax, yend = .05*yMax, linetype=2, color = "firebrick", alpha = .75) +
      annotate("text", x =  ci[1]-.05*dataRange, y = .1*yMax, vjust = 1, hjust = "left:", label = "80% CI", color = "firebrick")
  }
  
  return(p)
  
}





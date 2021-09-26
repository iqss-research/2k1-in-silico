
############################################################
# Plotter
############################################################


continuousDistrPlotter <- function(distrDF, paramVal, paramTex,
                                   annotate = TRUE,
                                   annotationX = NULL,
                                   arrow = TRUE,
                                   roundDigits = 1,
                                   discreteOutput = FALSE,
                                   xlims = NULL,
                                   ylims = NULL,
                                   plotColor = "steelblue"){
  
  if(is.null(annotationX)){annotationX <- mean(distrDF$drawVal)}
  if(is.null(xlims)){xMinVal <- min(distrDF$drawVal); xMaxVal <- max(distrDF$drawVal) 
  } else {xMinVal <- xlims[1]; xMaxVal <- xlims[2]}  
  if(is.null(ylims)){yMinVal <- 0; yMaxVal <- max(distrDF$prob)*1.1
  } else {yMinVal <- ylims[1]; yMaxVal <- ylims[2]}
  
  paramVal <- as.numeric(paramVal)
  annotationX <- as.numeric(annotationX)
  
  p <- ggplot() +
    geom_line(mapping = aes(x = distrDF$drawVal, y = distrDF$prob), color = plotColor , size = 1) +
    labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
    xlim(xMinVal, xMaxVal) +
    ylim(yMinVal, yMaxVal) + 
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
  
  if(discreteOutput){p <- p + geom_point(mapping = aes(x = distrDF$drawVal, y = distrDF$prob),
                                         color = plotColor,  size = 3, shape = "square")}
  
  
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

histogramMaker <- function(
  data,
  title = "",
  greaterThan = 999,
  annotate = F,
  captionText = NULL,
  ci = NULL,
  border = T,
  nBinsOverride = 40,
  xlims = NULL){
  
  errMessage <- "No data received. Please refresh or change incorrect parameters and try again."
  if(!is.numeric(data) ||! is.null(ncol(data))){
    return(ggplot() + annotate("text", x = 4, y = 1, size=4, label = paste(errMessage, collapse = " ")) + theme_void())}
  
  # if(is.null(xlims)){xMinVal <- min(data); xMaxVal <- max(data) 
  # } else {xMinVal <- xlims[1]; xMaxVal <- xlims[2]}  
  
  bordColor <- "steelblue" #if(border){"black"} else{"steelblue"}
  
  histData <- data.frame(value = data)   
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  # make sure bins include 1
  nBins <- min(nBinsOverride, length(unique(histData$value)))
  breaks <- unique(round(pretty.default(data, nBins),2))
  tmpVar <- 0
  while(length(breaks) != 0 && length(which(breaks==1)) ==0 && (max(breaks) > greaterThan)) {
    tmpVar <- tmpVar+1
    breaks <- breaks + (tmpVar*(-1)^(tmpVar-1)/100)
  }
  
  histData <- histData %>%  mutate(grtFlag = (value > greaterThan)) %>%  group_by(grtFlag)
  if(length(breaks) == 1) {breaks <- NULL}
  
  p <- ggplot() + 
    aes(x = histData$value, fill = histData$grtFlag) + 
    geom_histogram(
      data = histData,
      aes(y=100*..count../sum(..count..)),
      breaks = breaks,bins = 30, alpha = 0.5,color = bordColor, position = "identity") +
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
  
  if(!is.null(xlims)) {p <- p + xlim(xMinVal, xMaxVal)}
  
  return(p)
  
}



histAndDensity <- function(data, domain, pdfFun, assumedParam, binWidthVal = .5, multiModel = F){
  
  histData <- tibble(value = data)
  
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  if(multiModel == F){functionFun <- pdfFun; assumedParam <- assumedParam[1]} else{
    # TODO: remove code repetition
    allModels <- apply(assumedParam , 1, function(a){
      function(b){pdfFun(drawVal = b, param = a)}
    })
    # for each model, here are our y values
    drawVals <- seq(domain[1], domain[2], .01)
    
    allDensities <- lapply(allModels, function(m){m(drawVals)}) 
    allDensitiesMat <- allDensities %>%  unlist %>%  matrix(ncol = length(drawVals), byrow = T)
    sumDensities <- colMeans(allDensitiesMat)
    
    analyticalDistr <- data.frame(drawVal = drawVals, prob = sumDensities)

    functionFun <- function(q, z){
      # TODO: why is this so ugly
      sapply(q, function(r) {analyticalDistr$prob[which.min(abs(analyticalDistr$drawVal-r))]})
    }
  }
  # browser()
  ggplot(histData, aes(x = value)) +
    geom_histogram(aes(y=100*..count../sum(..count..)), bins = 20,
                   color = "steelblue", fill = "steelblue") +
    xlim(domain[1], domain[2]) +
    stat_function(fun = function(a){100*functionFun(a,assumedParam)}, color = "firebrick", size = 1) +
    labs(x = "y", y = "Observed Density")+
    scale_y_continuous(labels = scaleFUN, breaks = seq(0, 1000, 10), limits = c(0, 75)) + 
    theme_minimal() +
    theme(legend.position = "none",
          plot.caption = element_text(size=12, margin = ggplot2::margin(t = 10), hjust = 0.5),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    )
  
  
}



histAndDensityBinary <- function(data, domain, pdfFun, assumedParam, binWidthVal = .5, multiModel = F){
  
  assumedParam <- mean(assumedParam)
  observed <- tibble(group = "observed", drawVal = c(0,1), probs = c(sum(1-data), sum(data))/length(data))
  hypothesized <- tibble(group = "hypothesized", drawVal = c(0,1), probs = c(1-assumedParam, assumedParam))
  
  histData <- rbind(observed, hypothesized)
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  ggplot(histData, aes(x = drawVal, y = probs, fill = group, color = group)) +
    geom_bar(stat="identity", alpha = .25, position = "identity") +
    scale_fill_manual(values=c("firebrick","steelblue")) +
    scale_color_manual(values=c("firebrick","steelblue")) +
    theme_minimal() +
    labs(x = "y", y = "Observed Probability") +
    ylim(0,max(1, histData$probs[1] + .2)) +
    theme(legend.position = "none",
          plot.caption = element_text(size=12, margin = ggplot2::margin(t = 10), hjust = 0.5),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) 
  
  
}


multiModelDensity <- function(param, domain, pdf, ...){
  
  # PDF takes arguments (drawVal, param)
  
  # Here, param is a 1 or 2 x nObs vector
  # Use it to create a list of models
  allModels <- apply(param, 1, function(a){
    function(b){pdf(drawVal = b, param = a)}
  })
  # for each model, here are our y values
  drawVals <- seq(domain[1], domain[2], .01)
  
  allDensities <- lapply(allModels, function(m){m(drawVals)}) 
  allDensitiesMat <- allDensities %>%  unlist %>%  matrix(ncol = length(drawVals), byrow = T)
  sumDensities <- colMeans(allDensitiesMat)
  
  analyticalDistr <- data.frame(drawVal = drawVals, prob = sumDensities)
  
  continuousDistrPlotter(analyticalDistr, xlims = domain, ...)  
}


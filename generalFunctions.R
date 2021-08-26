

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


### TODO merge these print functions
decPrintHelper <- function(header, data, printLength){
  
  if(length(data) > printLength){truncData <- data[1:printLength]}
  else{truncData <- data}
  charData <- lapply(truncData, function(s){sprintf("%0.1f",s)}) %>%  unlist()
  
  printStr <- paste(c(charData), collapse = ", ")
  printStr <- paste(header, printStr, sep = "")
  if(length(data) > printLength){printStr <- paste0(printStr, " ...")}
  
  printStr
}


intPrintHelper <- function(header, data, printLength){
  
  printStr <- paste(c(header, data), sep = " ")
  if(length(data) > printLength){printStr <- paste0(printStr, " ...")}
  printStr <-paste(printStr, collapse = " ")
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
  
  
  ggplot(distrDF, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity", alpha = .75) +
    scale_fill_manual(values=c(plotColor1, plotColor2)) +
    labs(x= "y", y = TeX(paste0("P$(y|", paramTex, ")$"))) +
    theme_minimal() +
    ylim(0,1) +
    theme(text = element_text(family = "sans"),
          legend.position = "none",  
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"))
    ) + annotate("text", x = 0.75, y = .9,
                 label  = parse(
                   text=TeX(paste0("$",paramTex,"$","=",round(paramVal, roundDigits)), output = "character")),
                 parse = TRUE, color = "black", size = 6, fontface = "bold")
  
  
  
}

### takes a vector

histogramMaker <- function(data, title = "", greaterThan = 999, annotate = F, captionText = NULL){
  errMessage <- "No data received. Please refresh and try again."
  if(!is.numeric(data)){return(ggplot() + annotate("text", x = 4, y = 1, size=4, label = paste(errMessage, collapse = " ")) + theme_void())}
  
  histData <- data.frame(value = data)   
  dataMean <- mean(data, na.rm = TRUE)
  dataSD <- sd(data, na.rm = TRUE)
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  # make sure bins include 1
  nBins <- min(40, length(unique(histData$value)))
  breaks <- pretty.default(data, nBins)
  tmpVar <- 0
  while(length(breaks) != 0 && length(which(breaks==1)) ==0) {
    tmpVar <- tmpVar+1
    breaks <- breaks + tmpVar*(-1)^(tmpVar-1)
  }
  histData <- histData %>%  mutate(grtFlag = (value > greaterThan)) %>%  group_by(grtFlag)
  
  p <- ggplot(histData) + 
    aes(x = value, fill = grtFlag) +
    geom_histogram(
      aes(y= ..count../sum(..count..)), breaks = breaks,color = "black", alpha = 0.5, position = "identity") +
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
  
  if(annotate){
    p <- p + annotate("text", x = dataMean, y = Inf, vjust = 1, hjust = "left",
                      label  = paste0("Mean: ", round(dataMean,1 ),"; SE:", round(dataSD,1 )), color = "black") + 
      annotate("segment", x = dataMean, y = Inf, xend = dataMean, yend = 0, color = "black")}
  
  if(!is.null(captionText)){
    p <- p + labs(caption = captionText)
  }
  
  return(p)
  
}


##############################################
# distribution Tex
##############################################

distrLatexFunction <- function(
  type, 
  modelName,
  pdfTex, 
  modelDistTex, 
  modelParamTex, 
  likelihoodTex, 
  logLikelihoodTex,
  smallLik = F,
  smallLL = F){
  
  smallLikTex <- if(smallLik){"\\small"} else {""}
  smallLLTex <- if(smallLL){"\\small"} else {""}
  
  if(type == "Distr"){
    
    tags$p(withMathJax(paste0("\\(",pdfTex,"\\)")))
    
  }
  else if(type == "Model"){
    
    div(tags$p(withMathJax("Statistical Model: ")),
        tags$p(paste0("\\( \\hspace{30px}", modelDistTex,"\\)")),
        tags$p(paste0("\\( \\hspace{30px}", modelParamTex,"\\)")),
        tags$p("\\( \\hspace{30px} Y_i \\perp \\!\\!\\! \\perp Y_j \\quad \\forall \\: i \\neq j \\)"))
    
  } else if(type == "Likelihood"){
    
    div(tags$p(tags$b(withMathJax("Likelihood given data \\(\\small y = (y_1, \\dots,y_n)\\) :"))),
        tags$p(paste0(" \\(\\hspace{30px}{",smallLikTex,likelihoodTex,"}\\)")),
        tags$p(tags$b("Log Likelihood:")),
        tags$p(paste0("\\(\\hspace{30px}{", smallLLTex, logLikelihoodTex," } \\)")))
    
  } else stop("Unknown Markdown!")
  
  
  
}
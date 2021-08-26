source("preamble.R")

data <- rlnorm(1000, 1.5)
histData <- data.frame(value = data, 1)  
nBins <- min(40, length(unique(histData$value)))
breaks <- pretty.default(data, nBins)
j <- 0
print(breaks)
histogramMaker(data, greaterThan = 1)

histogramMaker <- function(data, title ="", greaterThan = 999, annotate = F, captionText = NULL){
  
  histData <- data.frame(value = data)   
  dataMean <- mean(data, na.rm = TRUE)
  dataSD <- sd(data, na.rm = TRUE)
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  # make sure bins include 1
  nBins <- min(40, length(unique(histData$value)))
  breaks <- pretty.default(data, nBins)
  j <- 0
  while(length(which(breaks==1)) ==0) {
    j <- j+1
    breaks <- breaks + j*(-1)^(j-1)
  }
  print(breaks)
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

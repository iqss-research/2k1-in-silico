############################################################
# Every function on this page should take one argument,
# y tilde (simulated y). 
#
# The function should return a ggplot object and a table
# Either can be empty. 
#
# The function name should be listed with the QOI name
# in QOIList.xlsx
############################################################


probabilityGreaterThanOutputs <- function(yTilde){
  
  # table of probability that y > x
  
  pctiles <- seq(0,1,0.1)
  thresholds <- round(quantile(yTilde, pctiles),2)
  
  probs <- sapply(thresholds, function(a){sum(yTilde > a)/length(yTilde)})
  
  tbl <- DT::datatable(data.frame(pctiles,thresholds,probs),
                       rownames = F, 
                       colnames = c("Percentiles", "Thresholds", "Probability Y Above Threshold"),
                       options = list(pageLength = 12, lengthChange = F, paging = FALSE, searching = FALSE, dom = "t")) %>% 
    formatPercentage(columns = c("pctiles", "probs"))
  
  cht <- element_blank()
  list(table = tbl, chart = cht )
  
  
}


ySquaredOutputs <- function(yTilde){
  
  # Simple 
  yTildeSquared <- yTilde^2
  
  pctiles <- seq(0,1,0.1)
  probs <- round(quantile(yTildeSquared, pctiles),2)
  
  print(data.frame(pctiles,probs))
  tbl <- DT::datatable(data.frame(pctiles,probs),
                       rownames = F, 
                       colnames = c("Percentiles", "Value of Y Squared"),
                       options = list(
                         pageLength = 12, lengthChange = F, paging = FALSE,
                         searching = FALSE, dom = "t")) %>% 
    formatPercentage(columns = c("pctiles", "probs"))
 
  
  minY <- round(min(yTildeSquared),1)
  maxY <- round(max(yTildeSquared),1)
  stepY <- round(abs(maxY- minY)/8,1)
  
  print( c(seq(minY, maxY, stepY), maxY))
  
  histData <- data.frame(value = yTildeSquared)  %>%
    mutate(bin = cut(value,  unique(c(seq(minY, maxY, stepY), maxY), right = FALSE))) %>%  count(bin)  
  
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  cht <- ggplot(histData) +
    aes(x = bin, y = 100*n/sum(n)) +
    geom_col( width = .75) +
    geom_text(aes(x = bin, y = 1.5+100*n/sum(n), label = sprintf(fmt = "%0.1f%%", 100*n/sum(n)) )) +
    scale_y_continuous(labels = scaleFUN, breaks = seq(0, 100, 10))  + 
    theme_minimal()+
    xlab("Simulated y squared") +
    ylab(element_blank()) +
    labs(title = "", caption = "") +
    theme(plot.title = element_text(size=12, hjust  = .5, margin = ggplot2::margin(b = 10)),
          plot.caption = element_text(size=7 , margin = ggplot2::margin(t = 10)),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(margin = ggplot2::margin(t = 6)))  
  
  
  list(table = tbl, chart = cht )
  
}
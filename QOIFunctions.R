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
  thresholds <- round(quantile(yTilde, pctiles, na.rm = T),2)
  
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
  probs <- round(quantile(yTildeSquared, pctiles, na.rm = T),2)
  
  tbl <- DT::datatable(data.frame(pctiles,probs),
                       rownames = F, 
                       colnames = c("Percentiles", "Value of Y Squared"),
                       options = list(
                         pageLength = 12, lengthChange = F, paging = FALSE,
                         searching = FALSE, dom = "t")) %>% 
    formatPercentage(columns = c("pctiles"))
 
  
  cht <- histogramMaker(yTildeSquared, "Simulated Y Squared")
  
  
  list(table = tbl, chart = cht )
  
}
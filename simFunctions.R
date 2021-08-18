############################################################
# Functions to help with simulation
############################################################

yTildeCreator <- function(paramHat, #\hat{\gamma}
                          paramVCov, #\hat{V}(\hat{\gamma})
                          model, # draws function - takes params, returns y
                          nSimDraws,
                          xRow = 1){ #X_c
  
  #get lots of parameters
  paramTilde <- rmvnorm(nSimDraws, paramHat, as.matrix(paramVCov))
  
  # \tilde{y}_c 
  yTilde <- sapply(1:nSimDraws, function(a){model(paramTilde[a,], 1, xRow)})
  
}



simHist <- function(yTilde){
  
  minY <- round(min(yTilde),1)
  maxY <- round(max(yTilde),1)
  stepY <- round(abs(maxY- minY)/8,1)
  
  histData <- data.frame(value = yTilde)  %>%
    mutate(bin = cut(value,  c(seq(minY, maxY, stepY), maxY), right = FALSE)) %>%  count(bin) 
  
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  ggplot(histData) +
    aes(x = bin, y = 100*n/sum(n)) +
    geom_col( width = .75) +
    geom_text(aes(x = bin, y = 1.5+100*n/sum(n), label = sprintf(fmt = "%0.1f%%", 100*n/sum(n)) )) +
    scale_y_continuous(labels = scaleFUN, breaks = seq(0, 100, 10)) +
    xlab("y") +
    ylab(element_blank()) +
    labs(title = "", caption = "") +
    theme(plot.title = element_text(size=12, hjust  = .5, margin = ggplot2::margin(b = 10)),
          plot.caption = element_text(size=7 , margin = ggplot2::margin(t = 10)),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(margin = ggplot2::margin(t = 6)))
  
  
  
}


QOITables <- function(yTilde, QOIName){
  
  if(QOIName == "probGrt"){
    
    # table of probability that y > x
    
    pctiles <- seq(0,1,0.1)
    thresholds <- round(quantile(yTilde, pctiles),2)
    
    probs <- sapply(thresholds, function(a){sum(yTilde > a)/length(yTilde)})
      
    
    ret <- DT::datatable(data.frame(pctiles,thresholds,probs),
                  rownames = F, 
                  colnames = c("Percentiles", "Thresholds", "Probability Y Above Threshold"),
                  options = list(pageLength = 12, lengthChange = F, paging = FALSE, searching = FALSE, dom = "t")) %>% 
      formatPercentage(columns = c("pctiles", "probs"))

  } else {error("Unknown QOI")}
  
  ret
  
}

############################################################
# Functions to help with simulation
############################################################

yTildeCreator <- function(paramHat, #\hat{\gamma}
                          paramVCov, #\hat{V}(\hat{\gamma})
                          model, # draws function - takes params, returns y
                          nSimDraws,
                          xRow = 1){ #X_c
  
  #get lots of parameters
  paramTilde <- tryCatch({rmvnorm(nSimDraws, paramHat, as.matrix(paramVCov))}, 
                         error = function(e){
                           matrix(rep(NA,nSimDraws*length(paramHat)), nrow = nSimDraws)
                        })
  
  # \tilde{y}_c 
  yTilde <- sapply(1:nSimDraws, function(a){model(paramTilde[a,], 1, xRow)})
  
}



simHist <- function(yTilde){
  
  if(all(is.na(yTilde))){
     p <- stop("No y data found. Check that your MLE has an invertible Hessian.")
  }
  else{ p <-  tryCatch({
  minY <- round(min(yTilde),1)
  maxY <- round(max(yTilde),1)
  stepY <- round(abs(maxY- minY)/8,1)
  
  if(all(unique(yTilde)== c(0,1))||all(unique(yTilde)== c(1,0))){ # detects binary output    
    histData <- data.frame(value = yTilde)  %>%
      mutate(bin = cut(value,  c(0,1,2), right = FALSE)) %>%  count(bin)   
    
  } else {
    histData <- data.frame(value = yTilde)  %>%
      mutate(bin = cut(value,  c(seq(minY, maxY, stepY), maxY), right = FALSE)) %>%  count(bin)   
  }
  
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  
  p <- ggplot(histData) +
    aes(x = bin, y = 100*n/sum(n)) +
    geom_col( width = .75) +
    geom_text(aes(x = bin, y = 1.5+100*n/sum(n), label = sprintf(fmt = "%0.1f%%", 100*n/sum(n)) )) +
    scale_y_continuous(labels = scaleFUN, breaks = seq(0, 100, 10))  + 
    theme_minimal()+
    xlab("Simulated y") +
    ylab(element_blank()) +
    labs(title = "", caption = "") +
    theme(plot.title = element_text(size=12, hjust  = .5, margin = ggplot2::margin(b = 10)),
          plot.caption = element_text(size=7 , margin = ggplot2::margin(t = 10)),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(margin = ggplot2::margin(t = 6)))},
  error = function(e){stop("Error in drawing histogram. Please refresh or try different parameters.")})
  
  }
  
  return(p)
  
}


QOIVisualization <- function(yTilde, QOIName){
  errMessage <- "Error in computing QOI. Please make sure your simulated variables exist."
  
  ret <- tryCatch({
  idx <- which(QOIDF$Name==QOIName)
  
  if(length(idx) > 0){
    f <- eval(parse(text=QOIDF$FunctionName[[idx]]))
    return(f(yTilde))
  } else{stop("Unknown QOI!")}},
  error = function(e){stop(e)})
  
  ret
  
}

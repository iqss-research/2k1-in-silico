
pdf <- styNormPDF
nObs <- 200
data <- styNormDraws(.5, nObs)
assumedParam <- .6
domain <- c(-4,4)
nBins <- 20
histData <- tibble(value = data)

ggplot(histData, aes(x = value)) +
  geom_histogram(aes(y=100*1/binWidthVal*..count../sum(..count..)),
                 binwidth= binWidthVal, color = "steelblue", fill = "steelblue") +
  xlim(domain[1], domain[2]) +
  stat_function(fun = function(a){100*pdf(a,assumedParam)}, color = "firebrick", size = 1.5) +
  theme(legend.position = "none",
        plot.caption = element_text(size=12, margin = ggplot2::margin(t = 10), hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size=12, margin = ggplot2::margin(t = 6)))  
 


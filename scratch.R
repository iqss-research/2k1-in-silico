
pdf <- styNormPDF
nObs <- 200
data <- styNormDraws(.5, nObs)
assumedParam <- rnorm(200, .6) %>%  as.matrix
domain <- c(-4,4)
nBins <- 20
multiModel <- T

histData <- tibble(value = data)

scaleFUN <- function(x) sprintf("%.0f%%", x)

# TODO: remove code repetition
allModels <- apply(assumedParam, 1, function(a){
  function(b){pdf(drawVal = b, param = a)}
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


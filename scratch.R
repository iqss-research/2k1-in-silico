source("preamble.R")
library(plotly)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotlyOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    # set.seed(2001)
    # steps <- seq(-1,1,.1) %>% as.list()
    # rawData <- data.frame(y = rnorm(200), z = seq(-3,3,length.out=200)) 
    # nBins <- 20
    # 
    # xData <- lapply(steps, function(a){
    #   data.frame(x = seq(-3,3,length.out = 61)) %>%  
    #     mutate(prob = (2*pi)^(-1/2)* exp(-(1/2)* (x - a)^2))  %>% 
    #     mutate(prob = prob/sum(prob)*nBins)
    # })
    # 
    # fig <- plot_ly(rawData, x = ~y) %>%  
    #   add_trace(type = "histogram", histnorm = "probability", xbins = nBins) 
    # 
    # for(i in 1:length(steps)){
    #   plotData <- xData[[i]]
    #   fig <- fig %>% add_lines(x = plotData$x, y = plotData$prob)
    #   
    # }
    # 
    # 
    # # muVals <- 2*rbernoulli(200,.5)
    # # plotData <- data.frame(y = rnorm(muVals) )
    # 
    # plot_ly(plotData, x = ~y) %>%  add_trace(type = "histogram", histnorm = "probability", xbins = 20) %>% 
    #   add_trace(x = ~z, y = ~prob, name = 'trace 0', type = 'scatter', mode = 'markers')
    # 
    
    nObs  <- 200
    nBins <- 50
    nSteps <- 11
    xMax <- 6
    xMin <- -6
    
    x <- seq(xMin,xMax, length.out = 5*nBins)
    rawData <- data.frame(y = rnorm(nObs, mean = 1))
    
    styNormPDF <- function(yVal, muVal){
      (2*pi)^(-1/2)* exp(-(1/2)* (yVal - muVal)^2) 
    }
    
    stepMeans <- seq(-2,2,length.out = nSteps)
    # create data
    pdfVals <- list()
    for(step in 1:nSteps){
      stepMean <- stepMeans[step]
      pdfVals[[step]] <-list(visible = FALSE,
                          name = paste0('mu = ', stepMean),
                          x=x,
                          y=5*styNormPDF(x,stepMean)/sum(styNormPDF(x,stepMean)))
    }
    pdfVals[[3]]$visible = TRUE
    
    # create steps and plot all traces
    steps <- list()
    fig <- plot_ly()#height = 500, width = 500)
    for (i in 1:nSteps) {
      fig <- add_lines(fig,x=pdfVals[[i]]$x,  y=pdfVals[[i]]$y, visible = pdfVals[[i]]$visible, 
                       name = pdfVals[[i]]$name, type = 'scatter', mode = 'lines', hoverinfo = 'name', 
                       line=list(color='00CED1'), showlegend = FALSE)
      
      step <- list(args = list('visible', c(rep(FALSE, length(pdfVals)),TRUE)),
                   method = 'restyle', label = stepMeans[i])
      step$args[[2]][i] = TRUE  
      steps[[i]] = step 
    }  
    
    # add slider control to plot
    fig <- fig %>%
      add_trace(x = rawData$y,
                type = "histogram", histnorm = "probability", nbinsx = nBins, showlegend = FALSE) 
    
    fig2 <- mlEstimation  
      
      
      
    compositeFig <- subplot(fig1, fig2, nrows =2) %>% 
      layout(sliders = list(list(active = 3,
                                 currentvalue = list(prefix = "Mean: "),
                                 steps = steps)))  
    
    compositeFig
    
      
  })
  
  
  
}


shinyApp(ui = ui, server = server)
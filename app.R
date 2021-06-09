

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

bernDraws <- function(piParam, nTrials){
    
    random <- runif(nTrials) # n i.i.d. uniform draws
    outcome <- ifelse(random <= piParam, 1, 0) # how many < pi
    
    # cat(outcome, "\n")
    
    return(outcome)
}

bernMLE <- function(outcome, intervals = 20){
    
    nVal <- length(outcome) # Turn number of successes into K
    nSuccesses <- sum(outcome)
    
    testPiParam <- (1:intervals)/intervals
    probOutcomeGivenPi <- log((testPiParam^(nSuccesses))*((1-testPiParam)^(nVal - nSuccesses)))
    
    return <- data.frame(Pi = testPiParam, Likelihood = probOutcomeGivenPi)
    
}



# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),

    # Application title
    titlePanel("Bernoulli Distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("piParam",
                        "Pi (Probability of Success):",
                        min = 0,
                        max = 1,
                        value = .5,
                        step = .1),
            sliderInput("nTrials",
                        "Number of Observations:",
                        min = 1,
                        max = 100,
                        value = 50,
                        step = 1),
            uiOutput("distr"),
            uiOutput("likelihood"),
            uiOutput("logLikelihood")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("MLEPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        analyticalDistr <- data.frame(
            drawVal = factor(c("Successes", "Failures"), levels = c("Successes", "Failures")),
            prob = c(input$piParam, 1-input$piParam)
        )
        
        ggplot(analyticalDistr, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity") +
            scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
            labs(title = "Distribution", x= "y", y = "P(y)")+
            theme_minimal() +
            theme(legend.position = "none")
        
    })
    
    
    output$MLEPlot <- renderPlot({
        outcome <- bernDraws(piParam = input$piParam, nTrials = input$nTrials) 
        
        likelihoodDB <- bernMLE(outcome = outcome, intervals = 100) %>% rename(`Log Likelihood` = Likelihood)
         
        ggplot(likelihoodDB, aes(x = Pi, y = `Log Likelihood`)) + geom_line(color = "steelblue") +
            theme_minimal()
        
    })
    
    output$distr <- renderUI({
        withMathJax(helpText("Bernoulli PMF: $$P(y|\\pi) = \\pi^y(1-\\pi)^{{(1-y)}}$$"))
    })
    
    output$likelihood <- renderUI({
        withMathJax(helpText("Likelihood given data \\( y = (y_1, \\dots,y_n)\\) : $$P(\\pi|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\pi^{y_i}(1-\\pi)^{{(1-y_i)}}$$"))
    })
    
    output$logLikelihood <- renderUI({
        withMathJax(helpText("Log Likelihood: $$\\ln[P(\\pi|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln(\\pi)$$ $$  + \\sum_{i=1}^{n} (1-y_i) \\ln(1-\\pi)$$"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

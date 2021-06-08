

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
    probOutcomeGivenPi <- (testPiParam^(nSuccesses))*((1-testPiParam)^(nVal - nSuccesses))
    
    return <- data.frame(Pi = testPiParam, Likelihood = probOutcomeGivenPi)
    
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bernoulli Distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("piParam",
                        "Probability of Success:",
                        min = 0,
                        max = 1,
                        value = .5,
                        step = .1),
            sliderInput("nTrials",
                        "Number of Trials:",
                        min = 1,
                        max = 50,
                        value = 20,
                        step = 1)
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
        # generate bins based on input$bins from ui.R
        outcome <- bernDraws(piParam = input$piParam, nTrials = input$nTrials) 
        draws <- data.frame(drawVal = ifelse(outcome, "Success", "Failure") %>%  factor(levels = c("Success", "Failure")))
        summaryDraws <- draws %>%  count(drawVal, .drop = FALSE) 
        
        ggplot(summaryDraws, aes(x = drawVal, y = n, fill = drawVal)) + geom_bar(stat="identity") +
            scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
            theme_minimal() +
            theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
        
    })
    
    
    output$MLEPlot <- renderPlot({
        outcome <- bernDraws(piParam = input$piParam, nTrials = input$nTrials) 
        
        likelihoodDB <- bernMLE(outcome = outcome, intervals = 100)
         
        ggplot(likelihoodDB, aes(x = Pi, y = Likelihood)) + geom_line(color = "steelblue") +
            theme_minimal() 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

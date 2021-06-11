

require(shiny)
require(shinyBS)
require(shinyjs)
require(dplyr)
require(tidyr)
require(ggplot2)
require(DT)

source("BernoulliHelpers.R")
source("generalHelpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyjs::useShinyjs(),
    withMathJax(),
    
    # Application title
    titlePanel("Distribution Explorer"),
    
    selectInput(
        "distrID",
        "Select Distribution",
        c("Bernoulli")
    ),
    
    fluidRow(
        h3("Known Distribution", style = "padding:15px"),
        column(4,
               sliderInput("piParamDistr",
                           "Known Pi (Probability of Success):",
                           min = 0,
                           max = 1,
                           value = .5,
                           step = .1
               ),
               uiOutput("distr")
        ),
        
        column(6, 
               plotOutput("distPlot", height = "400px")
        )
    ),
    hr(),
    
    fluidRow(
        h3("Data Generation Process", style = "padding:15px"),
        column(4, sliderInput("piParam",
                              "Pi (Probability of Success):",
                              min = 0,
                              max = 1,
                              value = .5,
                              step = .1),
               sliderInput("nTrials",
                           "Number of Observations:",
                           min = 1,
                           max = 500,
                           value = 250,
                           step = 1),
               div(style="display:inline-block",
                   actionButton(inputId = "generateDataButton",
                                label = "Generate Data",
                                icon("play-circle")
                   ),
                   bsTooltip(
                       "generateDataButton", 
                       "After defining distribution parameters, press here to generate data and display a sample",
                       placement = "bottom", 
                       trigger = "hover"
                   )
                   
               )
        ),
        column(6,
               h4("Data Snapshot"),
               textOutput("outcomeDisplay"))
    ),
    hr(),
    fluidRow(
        h3("Likelihood", style = "padding:15px"),
        column(4,
               uiOutput("likelihood"),
               uiOutput("logLikelihood")
        ),
        column(6,
               plotOutput("MLEPlot", height = "400px")
               )
    )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$distPlot <- renderPlot({
        
        analyticalDistr <- data.frame(
            drawVal = factor(c("Successes", "Failures"), levels = c("Successes", "Failures")),
            prob = c(input$piParamDistr, 1-input$piParamDistr)
        )
        
        ggplot(analyticalDistr, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity") +
            scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
            labs(title = "Distribution (Analytically)", x= "y", y = "P(y)")+
            theme_minimal() +
            theme(legend.position = "none", aspect.ratio=.8)
        
    })

    
    observeEvent(
        eventExpr = {
            input$generateDataButton
        },
        handlerExpr = {
            
            outcomeData <<- bernDraws(piParam = input$piParam, nTrials = input$nTrials)
            
            # output$outcomeData <- outcomeData
            
            withCallingHandlers({
                shinyjs::html("outcomeDisplay", "")
                bernDataPrintHelper(outcomeData, 144)
            },
            message = function(m) {
                shinyjs::html(id = "outcomeDisplay", html = m$message, add = TRUE)
            })
        }
        
    )
    
    observeEvent(
        eventExpr = {
            input$generateDataButton
        },
        handlerExpr = {
            
            outcomeData <<- bernDraws(piParam = input$piParam, nTrials = input$nTrials)
            
            # output$outcomeData <- outcomeData
            
            output$MLEPlot <- renderPlot({
                outcome <- outcomeData
                
                likelihoodDB <- bernMLE(outcome = outcome, intervals = 100) %>% rename(`Log Likelihood` = Likelihood)
                
                ggplot(likelihoodDB, aes(x = Pi, y = `Log Likelihood`)) + geom_line(color = "steelblue") +
                    theme_minimal()
                
            })
        }
        
    )
    
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

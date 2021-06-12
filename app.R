

packages <- c("shiny", "shinythemes", "shinyBS", "shinyjs", "dplyr", "tidyr", "ggplot2", "DT", "bslib", "showtext")

oldw <- getOption("warn")
options(warn = -1)

package.check <- lapply(packages,FUN = function(x) {
        if (!require(x, character.only = TRUE)) {install.packages(x, dependencies = TRUE)}})

package.load <- lapply(packages, function(x){library(x, character.only = TRUE)})

options(warn = oldw)

if(!any(grepl(pattern = "(Open Sans)", sysfonts::font_files()[,3]))){
    font_add_google(name = "Open Sans", family = "open-sans")
}
showtext_auto()

source("BernoulliHelpers.R")
source("generalHelpers.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "Distribution Explorer",
    theme = bs_theme(
        version = 3,
        bootswatch = "yeti",
        primary = "#BF5803",
        "navbar-default-bg" = "#BF5803",
        ),
    
    tabPanel(
        title = "Probability",
        shinyjs::useShinyjs(),
        withMathJax(),
        fluidRow(
            column(4,
                   selectInput(
                       "distrID",
                       "Select Distribution",
                       c("Bernoulli")
                   )
            ), column(6,
                      uiOutput("distr", style = "padding-top:15px")
            )
        ),
        fluidRow(
            column(4,
                   sliderInput("piParam",
                               "Set Parameter:",
                               min = 0,
                               max = 1,
                               value = .3,
                               step = .1
                   )
            ),
            
            column(6,
                   h4("Visualized Distribution"),
                   plotOutput("distPlot", height = "300px", width = "75%")
            )
        ),
        hr(),
        
        fluidRow(
            column(4,
                   div(style="display:inline-block; padding-bottom:10px",
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
                   ),
                   br(),
                   sliderInput("nTrials",
                               "Number of Observations:",
                               min = 1,
                               max = 500,
                               value = 20,
                               step = 1)
            ),
            column(6,
                   textOutput("outcomeDisplay")
            )
        )
    ),
    tabPanel(
        title ="Likelihood",
        fluidRow(
            column(6,
                   textOutput("distrNameOutput"),
                   textOutput("outcomeDisplay2")
            ),
            style = "padding-bottom:10px"
        ),
        hr(),
        fluidRow(
            column(4,
                   uiOutput("likelihood"),
                   uiOutput("logLikelihood")
            ),
            column(6,
                   plotOutput("MLEPlot", height = "400px")
            )
        )
    ),
    id = "tabs"
    
)

# Define server logic required to draw a histogram
if(exists("outcomeData")){rm(outcomeData, envir = .GlobalEnv)}
if(exists("distrName")){rm(distrName, envir = .GlobalEnv)}

server <- function(input, output, session) {
    
    observeEvent(
        input$distrID,{
            distrName <<- input$distrID
        })
    
    
    output$distPlot <- renderPlot({
        
        analyticalDistr <- data.frame(
            drawVal = factor(c("Successes", "Failures"), levels = c("Successes", "Failures")),
            prob = c(input$piParam, 1-input$piParam)
        )
        
        ggplot(analyticalDistr, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity") +
            scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
            labs(x= "y", y = "P(y)")+
            theme_minimal() +
            theme(text = element_text(family = "open-sans"),
                  legend.position = "none",  axis.text = element_text(size = 15),
                  axis.title.x = element_text(size = 15, margin = unit(c(4, 0, 0, 0), "mm")),
                  axis.title.y = element_text(size = 15, margin = unit(c(4, 4, 4, 4), "mm"))
                  )
        
    })
    
    
    observeEvent(
        input$piParam,{
            withCallingHandlers({
                shinyjs::html("outcomeDisplay2", "")
                message("!--- No Data Generated Yet ---!")
            },
            message = function(m) {
                shinyjs::html(id = "outcomeDisplay2", html = m$message, add = TRUE)
            })
            
            withCallingHandlers({
                shinyjs::html("outcomeDisplay", "")
                message("!--- No Data Generated Yet ---!")
            },
            message = function(m) {
                shinyjs::html(id = "outcomeDisplay", html = m$message, add = TRUE)
            })
        })
    

    observeEvent(
        input$tabs,{
            if((input$tabs == "Likelihood") && (!exists("outcomeData"))){
                withCallingHandlers({
                    shinyjs::html("outcomeDisplay2", "")
                    message("!--- No Data Generated Yet ---!")
                },
                message = function(m) {
                    shinyjs::html(id = "outcomeDisplay2", html = m$message, add = TRUE)
                })}
            else if((input$tabs == "Probability") && (!exists("outcomeData"))){
                withCallingHandlers({
                    shinyjs::html("outcomeDisplay", "")
                    message("!--- No Data Generated Yet ---!")
                },
                message = function(m) {
                    shinyjs::html(id = "outcomeDisplay", html = m$message, add = TRUE)
                })}
        }
        
    )
    
    
    observeEvent(
        eventExpr = {
            input$generateDataButton
        },
        handlerExpr = {
            
            outcomeData <<- bernDraws(piParam = input$piParam, nTrials = input$nTrials)
            
            # output$outcomeData <- outcomeData
            
            withCallingHandlers({
                shinyjs::html("outcomeDisplay", "")
                bernDataPrintHelper(outcomeData, 100)
            },
            message = function(m) {
                shinyjs::html(id = "outcomeDisplay", html = m$message, add = TRUE)
            })
            
            
            withCallingHandlers({
                shinyjs::html("outcomeDisplay2", "")
                bernDataPrintHelper(outcomeData, 100)
            },
            message = function(m) {
                shinyjs::html(id = "outcomeDisplay2", html = m$message, add = TRUE)
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
    
    output$distrNameOutput <- renderText({distrName})
    
    output$distr <- renderUI({
        withMathJax(helpText("$${\\large P(y|\\pi) = \\pi^y(1-\\pi)^{{(1-y)}}}$$"))
    })
    
    output$likelihood <- renderUI({
        withMathJax(helpText("Likelihood given data \\( y = (y_1, \\dots,y_n)\\) :  $${\\large P(\\pi|y) = k(y) \\cdot \\prod_{i = 1}^{n} \\pi^{y_i}(1-\\pi)^{{(1-y_i)}}}$$"))
    })
    
    output$logLikelihood <- renderUI({
        withMathJax(helpText("Log Likelihood: $${\\large \\ln[P(\\pi|y)] \\, \\dot{=}\\,  \\sum_{i=1}^{n} y_i \\ln(\\pi) }$$ $${\\large   + \\sum_{i=1}^{n} (1-y_i) \\ln(1-\\pi)}$$"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

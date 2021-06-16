
packages <- c("shiny", "shinythemes", "shinyBS", "shinyjs", "dplyr", "tidyr", "ggplot2", "DT", "bslib", "ADtools", "grid")

oldw <- getOption("warn")
options(warn = -1)

package.check <- lapply(packages,FUN = function(x) {
    if (!require(x, character.only = TRUE)) {install.packages(x, dependencies = TRUE)}})

package.load <- lapply(packages, function(x){library(x, character.only = TRUE)})


options(warn = oldw)

source("BernoulliHelpers.R")
source("generalHelpers.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "Probability and Likelihood",
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
                   sliderInput("param",
                               "Set Parameter Pi:",
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
                   sliderInput("nObs",
                               "Number of Observations:",
                               min = 1,
                               max = 200,
                               value = 20,
                               step = 1),
                   br(),
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
                   htmlOutput("distrNameOutput", container = tags$b),
                   hr(),
                   textOutput("outcomeDisplay2")
            ),
            style = "padding-bottom:10px"
        ),
        hr(),
        fluidRow(
            column(4,uiOutput("statModel"))
            
        ),
        fluidRow(
            column(4,
                   uiOutput("likelihood")
            ),
            column(6,
                   plotOutput("MLEPlot", height = "400px")
            )
        )
    ),
    id = "tabs"
    
)


#######################################################################


if(exists("outcomeData")){rm(outcomeData, envir = .GlobalEnv)}
if(exists("distrName")){rm(distrName, envir = .GlobalEnv)}

server <- function(input, output, session) {
    
    observeEvent(
        input$distrID,{
            distrName <<- input$distrID
        })
    
    
    output$distrNameOutput <- renderUI({distrName})

    
    
    output$distPlot <- renderPlot({
        
        distrPlot(input$distrID, input$param)
        
    })
    
    
    observeEvent(
        input$tabs,{
            in_silence({
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
            })
        }
        
    )
    
    observeEvent(
        input$piParam,{
            in_silence({
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
        })
    
    
    
    observeEvent(
        eventExpr = {
            input$generateDataButton
        },
        handlerExpr = {
            
            outcomeData <<- bernDraws(piParam = input$param, nTrials = input$nObs)
            
            # output$outcomeData <- outcomeData
            in_silence({
                withCallingHandlers({
                    shinyjs::html("outcomeDisplay", "")
                    bernDataPrintHelper("<b>Data:</b>", outcomeData, 200)
                },
                message = function(m) {
                    shinyjs::html(id = "outcomeDisplay", html = m$message, add = TRUE)
                })
                
                
                withCallingHandlers({
                    shinyjs::html("outcomeDisplay2", "")
                    bernDataPrintHelper("<b>Data from Probability Tab:</b>", outcomeData, 200)
                },
                message = function(m) {
                    shinyjs::html(id = "outcomeDisplay2", html = m$message, add = TRUE)
                })
            })
        }
        
    )
    
    observeEvent(
        eventExpr = {
            input$generateDataButton
        },
        handlerExpr = {
            
            outcomeData <<- bernDraws(piParam = input$param, nTrials = input$nObs)
            
            output$MLEPlot <- renderPlot({MLEPlot(input$distrID, outcomeData)})
        }
        
    )

    output$distr <- renderUI({latexSwitcher(input$distrID, type = "Distr")})
    
    
    output$statModel <- renderUI({latexSwitcher(input$distrID, type = "Model")})
    
    output$likelihood <- renderUI({latexSwitcher(input$distrID, type = "Likelihood")})


}

# Run the application 
shinyApp(ui = ui, server = server)

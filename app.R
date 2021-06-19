
packages <- c("shiny", "shinythemes", "shinyBS", "shinyjs", "dplyr", "tidyr", "ggplot2", "DT", "bslib", "ADtools", "grid")

oldw <- getOption("warn")
options(warn = -1)

package.check <- lapply(packages,FUN = function(x) {
    if (!require(x, character.only = TRUE)) {install.packages(x, dependencies = TRUE)}})

package.load <- lapply(packages, function(x){library(x, character.only = TRUE)})


options(warn = oldw)

source("BernoulliHelpers.R")
source("generalHelpers.R")
source("ui.R")



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
                        message("!--- Generate Data on Probability Tab ---!")
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
        input$param,{
    
            in_silence({
                withCallingHandlers({
                    shinyjs::html("outcomeDisplay2", "")
                    message("!--- Generate Data on Probability Tab ---!")
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
                    bernDataPrintHelper(paste0("<b>",distrName, "Data from Probability Tab:</b>"), outcomeData, 200)
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

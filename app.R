
packages <- c("shiny", "shinythemes", "shinyBS", "shinyjs", "dplyr", "tidyr", "ggplot2", "DT", "bslib", "ADtools", "grid")

oldw <- getOption("warn")
options(warn = -1)

package.check <- lapply(packages,FUN = function(x) {
    if (!require(x, character.only = TRUE)) {install.packages(x, dependencies = TRUE)}})

package.load <- lapply(packages, function(x){library(x, character.only = TRUE)})
options(warn = oldw)

source("generalHelpers.R")
source("ui.R")
sapply(list.files("DistributionSpecific/"), function(a)(source(paste0("DistributionSpecific/", a))))

#######################################################################


if(exists("outcomeData")){rm(outcomeData, envir = .GlobalEnv)}
if(exists("distrName")){rm(distrName, envir = .GlobalEnv)}

server <- function(input, output, session) {
    
    observeEvent(input$distrID,{distrName <<- input$distrID})
    
    
    output$distrNameOutput <- renderUI({distrName})
    
    output$paramSlider <- renderUI({paramSwitcher(input$distrID)})

    output$outcomeDisplayP <- renderText({outTextP()})
    
    output$outcomeDisplayL  <- renderText({outTextL()})
    
    output$distPlot <- renderPlot({try({distrPlot(input$distrID, input$param)}, silent = TRUE)})
    
    noDataStrP <- "!-----No Data Generated-----!"
    noDataStrL <- "!-----Generate Data on Probability Page-----!"
    
    outTextP <- reactiveVal(noDataStrP)
    outTextL <- reactiveVal(noDataStrL)
    
    observeEvent({
        # input$param
        # input$nObs
    },{
        outTextP(noDataStrP)
        outTextL(noDataStrL)
        
        output$MLEPlot <- renderPlot({geom_blank()})
    })
    
    observeEvent({
        # input$generateDataButton
        input$param
        input$distrID
        input$nObs
        },{
        outcomeData <- drawSwitcher(input$distrID, param = input$param, nObs = input$nObs)
        
        outTextP(dataPrintSwitcher(input$distrID, "<b>Data</b>: ", outcomeData))
        outTextL(dataPrintSwitcher(input$distrID, "<b>Data from Probability Tab: </b>", outcomeData))
        
        output$MLEPlot <- renderPlot({MLEPlot(input$distrID, outcomeData)})
    })

    
    
    output$distr <- renderUI({latexSwitcher(input$distrID, type = "Distr")})
    
    output$statModel <- renderUI({latexSwitcher(input$distrID, type = "Model")})
    
    output$likelihood <- renderUI({latexSwitcher(input$distrID, type = "Likelihood")})


}

# Run the application 
shinyApp(ui = ui, server = server)

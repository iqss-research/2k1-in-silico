
source("preamble.R")
#######################################################################



server <- function(input, output, session) {
    
    titleText <- reactiveVal("")
    
    output$distrNameOutput <- renderUI({titleText()})
    
    output$paramSlider <- renderUI({paramSwitcher(input$distrID)})

    output$outcomeDisplayP <- renderText({outTextP()})
    
    output$outcomeDisplayL  <- renderText({outTextL()})
    

    
    noDataStrP <- "!-----No Data Generated-----!"
    noDataStrL <- "!-----Generate Data on Probability Page-----!"
    
    outTextP <- reactiveVal(noDataStrP)
    outTextL <- reactiveVal(noDataStrL)
   
    observeEvent({input$distrID},{titleText(paste0(input$distrID, ": Probability"))})
        
    
    observeEvent({
        input$param
        input$distrID
        input$nObs
        },{
        if(!is.null(input$param)){
            
            
            output$distPlot <- renderPlot({try({distrPlot(input$distrID, input$param)}, silent = TRUE)})
            
            outcomeData <- drawSwitcher(input$distrID, param = input$param, nObs = input$nObs)
            
            outTextP(dataPrintSwitcher(input$distrID, "<b>Data</b>: ", outcomeData, 200))
            outTextL(dataPrintSwitcher(input$distrID, "<b>Data from Probability Tab: </b>", outcomeData, 200))
            
            output$MLEPlot <- renderPlot({MLEPlot(input$distrID, outcomeData)})
            
        }
            
    })

    
    
    output$distr <- renderUI({latexSwitcher(input$distrID, type = "Distr")})
    
    output$statModel <- renderUI({latexSwitcher(input$distrID, type = "Model")})
    
    output$likelihood <- renderUI({latexSwitcher(input$distrID, type = "Likelihood")})


}

# Run the application 
shinyApp(ui = ui, server = server)

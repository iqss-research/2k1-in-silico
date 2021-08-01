
source("preamble.R")
source("global.R")

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
    
    paramsToUse <- reactiveVal(c())
    distrChartNum <- reactiveVal(1)

    
    observeEvent({input$distrID},{
        
        output$marginalSelector1 <-  renderUI({ marginalSelectInput(nVarSwitcher(input$distrID), 1 )})
        output$marginalSelector2 <- renderUI({ marginalSelectInput(nVarSwitcher(input$distrID), 2 )})
    })
        
    observeEvent({
        input$param1
        input$param2
        input$param3
        input$param4
        input$param5
        input$distrID
        input$nObs
        },{
        if(!is.null(input$param1)){
            paramsToUse <- reactiveVal(c())
            listParser(nVarSwitcher(input$distrID), "paramsToUse( c(paramsToUse(), input$param?))", environment())
            
            output$distPlot <- renderPlot({try({
                distrPlot(input$distrID, paramsToUse(), input$marginalSelected1 %>%  as.integer())}, silent = TRUE)})
            
            outcomeData <- drawSwitcher(input$distrID, param = paramsToUse(), nObs = input$nObs)
            
            outTextP(dataPrintSwitcher(input$distrID, "<b>Data</b>: ",
                                       outcomeData, 200))
            outTextL(dataPrintSwitcher(input$distrID, "<b>Data from Probability Tab: </b>",
                                       outcomeData, 200))
            
            output$MLEPlot <- renderPlot({
                MLEPlot(input$distrID, outcomeData, input$marginalSelected2 %>% as.integer())})
            
        }
            
    })
    
    
    output$distr <- renderUI({latexSwitcher(input$distrID, type = "Distr")})
    
    output$statModel <- renderUI({latexSwitcher(input$distrID, type = "Model")})
    
    output$likelihood <- renderUI({latexSwitcher(input$distrID, type = "Likelihood")})


}

# Run the application 
shinyApp(ui = ui, server = server)

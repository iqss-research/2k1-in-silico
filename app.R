
source("preamble.R")

#######################################################################



server <- function(input, output, session) {
    
    shinyjs::addClass(id = "tabs", class = "navbar-right")
    
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
    marginalChoices <- reactiveVal()
    margNumTop <- reactiveVal()
    MLEVars <- reactiveVal(list())
    yTilde <- reactiveVal()
    paramTilde <- reactiveVal()
    outcomeData <- reactiveVal()
    QOIOutputs <- reactiveVal()
    xValsToUse <- reactiveVal(c())
    


    observeEvent({
        input$distrID
        input$param1
        input$param2
        input$param3
        input$param4
        input$param5
        input$nObs
        },{
        if(!is.null(input$param1) &&
           !is.null(eval(parse(text= paste0("input$param",(nVarSwitcher(input$distrID)))) )
        )){
            # TODO: figure out why the timing is not ideal
            
            paramsToUse <- reactiveVal(c())
            listParser(nVarSwitcher(input$distrID), "paramsToUse( c(paramsToUse(), input$param?))", environment())
            
            output$distPlot <- renderPlot({try({
                distrPlot(input$distrID, paramsToUse(), input$xRow %>%  as.integer())}, silent = F)})
            
            outcomeData(drawSwitcher(input$distrID, param = paramsToUse(), nObs = input$nObs))
            
            outTextP(dataPrintSwitcher(input$distrID, "<b>Data</b>: ",outcomeData(), 200))
            outTextL(dataPrintSwitcher(input$distrID, "<b>Data from Probability Tab: </b>",outcomeData(), 200))
            
            MLEVars(MLEPlot(input$distrID, outcomeData(), margNumTop()))
            
            output$MLEPlot <- renderPlot({MLEVars()$plot })
            
            # create n-1 sliders since x0 is constant
            output$simSliders <- renderUI({simMultiSliderFunction(nVarSwitcher(input$distrID)-1)})
            
        }
            
    })
    

    observeEvent({input$distrID},{
        
        marginalChoices(marginalsChoicesSwitcher(input$distrID))
        output$marginalSelector1 <-  renderUI({
            marginalSelectInput(nVarSwitcher(input$distrID), 1, marginalChoices())})
        output$marginalSelector2 <- renderUI({
            marginalSelectInput(nVarSwitcher(input$distrID), 2, marginalChoices())})
        removeUI(selector = '#xPrint')
        
    })
    
    observeEvent({input$xRow},{
        removeUI(selector = '#xPrint')
        insertUI(selector = '#placeholder',
                 ui = tags$div(
                     tags$p(paste0(c("X: ", sapply(
                         indepVarsBase[input$xRow %>%  as.integer(),1:nVarSwitcher(input$distrID)],
                         function(a){sprintf("%0.2f",a)})),collapse = ", ")),
                     id = "xPrint", style = "padding-top:15px"),
                 where = "afterEnd")
        
    })
    
        
    observeEvent({
        input$distrID
        input$param1
        input$param2
        input$param3
        input$param4
        input$param5
        input$nObs
        input$simX1
        input$simX2
        input$QOIid}, {
        
            if(!is.null(input$simX1) || nVarSwitcher(input$distrID) == 1){
            xValsToUse(c())
            listParser(nVarSwitcher(input$distrID), "xValsToUse( c(xValsToUse(), input$simX?))", environment())

            
            output$simDynamicLatex <- renderUI({simMathJaxDynamic(xValsToUse())})
            
            paramTilde(paramTildeCreator(paramHat = MLEVars()$paramHat,
                                         paramVCov =  MLEVars()$paramVCov,
                                         1000))
            
            yTilde(yTildeCreator(paramTilde(),
                                 model = modelSwitcher(input$distrID),
                                 xValsToUse()))
            
            QOIOutputs(QOIVisualization(yTilde(), paramTilde(), input$QOIid))
            output$QOIChart <- renderPlot({QOIOutputs()})
            }

    })
    
    
    observeEvent({input$marginalSelected2}, 
                 {
                     margNumTop(which(marginalsChoicesSwitcher(input$distrID)== input$marginalSelected2))
                     MLEVars(MLEPlot(input$distrID, outcomeData(), margNumTop()))
                 })

    
    
    output$distr <- renderUI({latexSwitcher(input$distrID, type = "Distr")})
    
    output$statModel <- renderUI({latexSwitcher(input$distrID, type = "Model")})
    
    output$likelihood <- renderUI({latexSwitcher(input$distrID, type = "Likelihood")})


}


# Run the application 
shinyApp(ui = ui, server = server,
         onStart = function(){
             oldw <<- getOption("warn")
             options(warn = -1)
             onStop(function(){
                 options(warn = oldw)
                 
             })
             
         })


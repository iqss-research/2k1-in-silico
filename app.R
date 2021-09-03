
source("preamble.R")

#######################################################################



server <- function(input, output, session) {
    
    shinyjs::addClass(id = "tabs", class = "navbar-right")
    
    titleText <- reactiveVal("")
    
    output$distrNameOutput <- renderUI({titleText()})
    
    output$paramSlider <- renderUI({paramSwitcher(input$distrID)})
    output$obsSlider <- renderUI({obsSliderFun(nVarSwitcher(input$distrID))})
    
    outTextP <- reactiveVal("!-----No Data Generated-----!")
    outTextL <- reactiveVal("!-----Generate Data on Probability Page-----!")
    outTextX1 <- reactiveVal("!-----No X1 Generated-----!")
    outTextX2 <- reactiveVal("!-----No X2 Generated-----!")
    
    output$outcomeDisplayP <- renderText({outTextP()})
    output$outcomeDisplayL  <- renderText({outTextL()})
    output$outcomeDisplayX1  <- renderUI({outTextX1()})
    output$outcomeDisplayX2  <- renderUI({outTextX2()})
    output$xChoiceDiv  <- renderUI({
        if(nVarSwitcher(input$distrID) > 1){xChoiceDivFun()} else{""}
    })
    
    output$statModel <- renderUI({latexSwitcher(input$distrID, type = "Model")})
    output$likelihood <- renderUI({latexSwitcher(input$distrID, type = "Likelihood")})
    
    output$pickQOIBox <- renderUI({QOISwitcher(input$distrID)})
    
    
    observeEvent({input$distrID},{titleText(paste0(input$distrID, ": Probability"))})
    
    paramsToUse <- reactiveVal(c())
    distrChartNum <- reactiveVal(1)
    marginalChoices <- reactiveVal()
    margNumTop <- reactiveVal()
    MLEVars <- reactiveVal(list())
    yTilde <- reactiveVal()
    paramTilde <- reactiveVal()
    muTilde <- reactiveVal()
    outcomeData <- reactiveVal()
    QOIOutputs <- reactiveVal()
    xValsToUse <- reactiveVal(c())
    
    
    ################################
    # Distribution and setup
    ################################
    
    observeEvent({
        input$distrID
        input$param1
        input$param2
        input$param3
        input$param4
        input$param5
        input$nObs
        input$xChoice1
        input$xChoice2
    },{
        if(!is.null(input$param1) &&
           !is.null(eval(parse(text= paste0("input$param",(nVarSwitcher(input$distrID)))) )
           )){
            # TODO: figure out why the timing is not ideal
            
            paramsToUse <- reactiveVal(c())
            xSummaryUI <- reactiveVal("")
            
            listParser(nVarSwitcher(input$distrID), "paramsToUse( c(paramsToUse(), input$param?))", environment())
            
            xVals <- if(nVarSwitcher(input$distrID) > 1){
                reactive({xValGenerator(input$nObs, c(input$xChoice1, input$xChoice2))})
            } else reactive({NULL})
            indepVarsBase <<- xVals() ## TODO: refactor MLE so this nonsense isn't needed
            output$xChoiceDiv   <- renderUI({
                if(nVarSwitcher(input$distrID) > 1){
                    xChoiceDivFun(xVals(), input$nObs, input$xChoice1, input$xChoice2)
                } else{""}})
            
            paramsTransformed <- reactive({
                sapply(1:input$nObs, function(i){transformSwitcher(input$distrID)(paramsToUse(), xVals()[i,])})  })
            output$distr <- renderUI({
                latexSwitcher(input$distrID, type = "Distr", paramValsPDF = paramsToUse(), nObs = input$nObs )})
            output$distPlot <- renderPlot({try({
                distrPlot(input$distrID, mean(paramsTransformed()))}, silent = F)})
            
            output$probHistPlot <- if(nVarSwitcher(input$distrID) > 1){
                renderPlot({
                    histogramMaker(
                        paramsTransformed(), paste0("Parameter $", paramTexLookup(input$distrID, meta = T), "$"))},
                    height = 400, width = 400)
            } else {
                renderPlot({element_blank()}, height = 1, width = 1)   
            }
            outcomeData(drawSwitcher(input$distrID, param = paramsTransformed(), nObs = input$nObs))
            
            outTextP(dataPrintSwitcher(input$distrID, "",outcomeData(), 200))
            outTextL(dataPrintSwitcher(input$distrID, "",outcomeData(), 200))
            
            
            # create n-1 sliders since x0 is constant
            output$simSliders <- renderUI({simMultiSliderFunction(nVarSwitcher(input$distrID)-1)})
            # print("step1 Complete")
        }
        
    })
    
    ################################
    # MLE UI and calculation
    ################################
    
    observeEvent({input$distrID},{
        
        marginalChoices(marginalsChoicesSwitcher(input$distrID))
        output$marginalSelector1 <-  renderUI({
            marginalSelectInput(nVarSwitcher(input$distrID), 1, marginalChoices())})
        output$marginalSelector2 <- renderUI({
            marginalSelectInput(nVarSwitcher(input$distrID), 2, marginalChoices())})
        removeUI(selector = '#xPrint')
        
    })
    
    
    
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
            
            margNumTop(which(marginalsChoicesSwitcher(input$distrID)== input$marginalSelected2))
            MLEVars(MLEPlot(input$distrID, outcomeData(), margNumTop()))
            
            output$MLEPlot <- renderPlot({MLEVars()$plot })
            
            # TODO: merge this nonsense into big TeX
            output$simParamLatex <- renderUI({
                simMLELatex(paste0("\\(\\hat{",paramTexLookup(input$distrID),"} =\\) "), MLEVars()$paramHat )})
            output$simVcovLatex <- renderUI({
                simMLELatex(
                    paste0("\\(\\hat{V}(\\hat{",paramTexLookup(input$distrID),"}) =\\) "), MLEVars()$paramVCov )})
            
            # print("step2 Complete")
            
        }
    })
    
    # TODO: figure out how to merge by tweaking all the reactivity
    observeEvent({input$marginalSelected2},{
        margNumTop(which(marginalsChoicesSwitcher(input$distrID)== input$marginalSelected2))
        MLEVars(MLEPlot(input$distrID, outcomeData(), margNumTop()))
        
        output$MLEPlot <- renderPlot({MLEVars()$plot })
        
        output$simParamLatex <- renderUI({
            simMLELatex(paste0("\\(\\hat{",paramTexLookup(input$distrID),"} =\\) "), MLEVars()$paramHat )})
        output$simVcovLatex <- renderUI({
            simMLELatex(
                paste0("\\(\\hat{V}(\\hat{",paramTexLookup(input$distrID),"}) =\\) "), MLEVars()$paramVCov )})
        
    })
    
    ################################
    # Simulation Calculations
    ################################
    
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
                if(nVarSwitcher(input$distrID) == 1){xValsToUse(c())}
                
                output$simEstimationLatex <-  renderUI({latexSwitcher(
                    input$distrID,
                    type = "Estimation Uncertainty",
                    paramTex = paramTexLookup(input$distrID)
                )})
                
                
                output$simFundamentalLatex <-  renderUI({latexSwitcher(
                    input$distrID,
                    type = "Fundamental Uncertainty",
                    xValsSim = xValsToUse(),
                    paramTex = paramTexLookup(input$distrID),
                    metaParamTex = paramTexLookup(input$distrID, meta = T),
                )})
                paramTilde(paramTildeCreator(paramHat = MLEVars()$paramHat,
                                             paramVCov =  MLEVars()$paramVCov,
                                             1000))
                
                muTilde(muTildeCreator(paramTilde(),
                                       transformSwitcher(input$distrID),
                                       xValsToUse()))
                
                yTilde(yTildeCreator(muTilde(),
                                     model = modelSwitcher(input$distrID)))
                
                QOIOutputs(QOIVisualization(yTilde(), muTilde(), input$distrID, input$QOIid))
                output$QOIChart <- renderPlot({QOIOutputs()})
                
                # print("step3 Complete")
            }
            
        })
    
    
    
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


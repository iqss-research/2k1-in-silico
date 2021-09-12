
source("preamble.R")

#######################################################################



server <- function(input, output, session) {
    
    session$allowReconnect("force") # this will stop it going grey, we hope
    
    shinyjs::addClass(id = "tabs", class = "navbar-right")
    
    titleText <- reactiveVal("")
    output$distrNameOutput <- renderUI({titleText()})
    # creates dynamic tab name
    observeEvent({input$distrID},{titleText(paste0(input$distrID, ": Probability"))})
    
    
    # sliders for top of 1st page
    output$paramSlider <- renderUI({paramSwitcher(input$distrID)})
    output$obsSlider <- renderUI({obsSliderFun(nVarSwitcher(input$distrID))})
    output$obsHeader <- renderUI({obsHeaderFun(nVarSwitcher(input$distrID))})
    # choices of assumption depend on actual
    output$assumedDistrSelect <- renderUI({assumedDistrSwitcher(input$distrID)})
    
    # printed data - shouldn't be visible
    outTextP <- reactiveVal("!-----No Data Generated-----!")
    outTextL <- reactiveVal("!-----Generate Data on Probability Page-----!")
    output$outcomeDisplayP <- renderText({outTextP()})
    output$outcomeDisplayL  <- renderText({outTextL()})
    
    
    ## Sets up default choices for X for 1st and 2nd pages
    output$xChoiceDiv  <- renderUI({
        if(nVarSwitcher(input$distrID) > 1){xChoiceDivFun()} else{
            xChoiceDivFun(hidden=T)}})
    output$assumedXChoiceDiv  <- renderUI({
        if(nVarSwitcher(input$assumedDistrID) > 1){xChoiceDivFun(assumed=T)} else{
            xChoiceDivFun(assumed=T, hidden = T)}})
    
    # TeX for MLE page
    output$statModel <- renderUI({latexSwitcher(input$distrID, type = "Model")})
    output$likelihood <- renderUI({latexSwitcher(input$distrID, type = "Likelihood")})
    
    # dynamic set of choices for Sim page
    output$pickQOIBox <- renderUI({QOISwitcher(input$assumedDistrID)})
    
    # set up some reactives
    paramsToUse <- reactiveVal(c())
    marginalChoices <- reactiveVal()
    margNumTop <- reactiveVal()
    MLEVars <- reactiveVal(list())
    yTilde <- reactiveVal()
    paramTilde <- reactiveVal()
    muTilde <- reactiveVal() #TODO: rename. this is the vector of "final parameters" eg. Xb
    outcomeData <- reactiveVal()
    QOIOutputs <- reactiveVal()
    xValsForSim <- reactiveVal(c())
    
    
    ################################
    # Distribution and setup
    ################################
    
    # TODO BIG: remove observeEvent logic and rely on reactive flow (draw diagram)
    observeEvent({
        input$distrID
        input$assumedDistrID
        input$param1
        input$param2
        input$param3
        input$nObs
        input$xChoice1
        input$xChoice2
    },{
        if(!is.null(input$param1) &&
           !is.null(eval(parse(text= paste0("input$param",(nVarSwitcher(input$distrID)))) )
           )){
            # TODO: figure out why the code is running twice. Probably reactivity
            
            # creates an object paramsToUse out of however many params there are
            # TODO: find out if there's a better way 
            paramsToUse <- reactiveVal(c())
            listParser(nVarSwitcher(input$distrID),
                       "paramsToUse( c(paramsToUse(), input$param?))", environment())
            
            # updates Xs based on user choice
            xVals <- if(nVarSwitcher(input$distrID) > 1){
                reactive({xValGenerator(input$nObs, c(input$xChoice1, input$xChoice2))})
            } else reactive({NULL})
            
            # updates the UI to print out the first few values of X May not be needed with standard Xs
            output$xChoiceDiv   <- renderUI({
                if(nVarSwitcher(input$distrID) > 1){
                    xChoiceDivFun(xVals(), input$nObs, input$xChoice1, input$xChoice2)
                } else{""}})
            
            # create the number of models we'll draw from. For non-covariates, they're all the same
            paramsTransformed <- reactive({
                sapply(1:input$nObs,
                       function(i){transformSwitcher(input$distrID)(paramsToUse(), xVals()[i,])})  })
            
            # density/mass TeX
            output$distrTex <- renderUI({
                latexSwitcher(input$distrID,
                              type = "Distr", paramValsPDF = paramsToUse(), nObs = input$nObs )})
            
            # analytical distr plot
            output$distPlot <- renderPlot({try({
                distrPlot(input$distrID, mean(paramsTransformed()))}, silent = F)})
            
            # histogram if covariates
            output$probHistPlot <- if(nVarSwitcher(input$distrID) > 1){
                renderPlot({
                    histogramMaker(paramsTransformed(),
                                   paste0("Parameter $", paramTexLookup(input$distrID, meta = T), "$"))},
                    height = 350, width = 350)
            } else {renderPlot({element_blank()}, height = 1, width = 1)}
            
            # generate and print Y
            outcomeData(drawSwitcher(input$distrID, param = paramsTransformed(), nObs = input$nObs))
            outTextP(dataPrintSwitcher(input$distrID, "",outcomeData(), 200))
            outTextL(dataPrintSwitcher(input$distrID, "",outcomeData(), 200))
            
            # create n-1 sliders for sim page since x0 is constant
            output$simSliders <- renderUI({simMultiSliderFunction(nVarSwitcher(input$distrID)-1)})
            # print("step1 Complete")
        }
        
    })
    
    ################################
    # MLE UI and calculation
    ################################
    
    # create profile likelihood selector
    observeEvent({
        input$distrID
        input$assumedDistrID},{
            marginalChoices(marginalsChoicesSwitcher(input$assumedDistrID))
            output$marginalSelector2 <- renderUI({
                marginalSelectInput(nVarSwitcher(input$assumedDistrID), 2, marginalChoices())})
            
        })
    
    
    observeEvent({
        input$distrID
        input$assumedDistrID
        input$param1
        input$param2
        input$param3
        input$param4
        input$param5
        input$nObs
        input$xChoice1
        input$xChoice2
        input$assumedXChoice1
        input$assumedXChoice2
        input$marginalSelected2
    },{
        if(!is.null(input$param1) &&
           !is.null(eval(parse(text= paste0("input$param",(nVarSwitcher(input$distrID)))) )
           )){
            
            ## Pick assumed values of X
            xValsAssumed <- reactive({xValGenerator(input$nObs, c(input$assumedXChoice1, input$assumedXChoice2))})
            
            ## this changes the state that likelihood functions will read. 
            ## TODO: refactor MLE and likelihoods so this is less dumb
            # indepVarsBase <<- xValsAssumed()  
            # print new assumed X
            output$assumedXChoiceDiv   <- renderUI({
                if(nVarSwitcher(input$assumedDistrID) > 1){
                    xChoiceDivFun(xValsAssumed(), input$nObs,
                                  input$assumedXChoice1, input$assumedXChoice2, assumed = T)
                } else{""}})
            
            # profile likelihood choice
            margNumTop(which(marginalsChoicesSwitcher(input$assumedDistrID)== input$marginalSelected2))
            
            # compute MLE variables and make plot
            MLEVars(MLESwitcher(input$assumedDistrID, 
                                outcome = outcomeData(), 
                                xVals = xValsAssumed(), 
                                margNum = margNumTop()))
            
            output$MLEPlot <- renderPlot({MLEVars()$plot})
            
            # TODO: merge this nonsense into big TeX
            # outputs of MLE results on p2 and p3
            output$simParamLatex <- renderUI({
                simMLELatex(paste0("\\(\\hat{",
                                   paramTexLookup(input$assumedDistrID),"} =\\) "), MLEVars()$paramHat )})
            output$simVcovLatex <- renderUI({
                simMLELatex(
                    paste0("\\(\\hat{V}(\\hat{",
                           paramTexLookup(input$assumedDistrID),"}) =\\) "), MLEVars()$paramVCov )})
            
            output$MLEParamLatex <- renderUI({
                simMLELatex(paste0("\\(\\hat{",
                                   paramTexLookup(input$assumedDistrID),"} =\\) "), MLEVars()$paramHat )})
            output$MLEVcovLatex <- renderUI({
                simMLELatex(
                    paste0("\\(\\hat{V}(\\hat{",
                           paramTexLookup(input$assumedDistrID),"}) =\\) "), MLEVars()$paramVCov )})
            
            
            # print("step2 Complete")
            
        }
    })
    
    ################################
    # Simulation Calculations
    ################################
    
    observeEvent({
        input$distrID
        input$assumedDistrID
        input$param1
        input$param2
        input$param3
        input$param4
        input$param5
        input$nObs
        input$xChoice1
        input$xChoice2
        input$assumedXChoice1
        input$assumedXChoice2
        input$simX1
        input$simX2
        input$QOIid}, {
            
            if((!is.null(input$simX1) || nVarSwitcher(input$assumedDistrID) == 1) &&
               length(MLEVars()) >0){ # TODO: fix
                
                # get Xs to use 
                xValsForSim(c())
                listParser(nVarSwitcher(
                    input$assumedDistrID), "xValsForSim( c(xValsForSim(), input$simX?))", environment())
                if(nVarSwitcher(input$assumedDistrID) == 1){xValsForSim(c())}
                
                # output TeX for est. and fundamental uncertainty
                output$simEstimationLatex <-  renderUI({latexSwitcher(
                    input$assumedDistrID,
                    type = "Estimation Uncertainty",
                    paramTex = paramTexLookup(input$assumedDistrID)
                )})
                
                output$simFundamentalLatex <-  renderUI({latexSwitcher(
                    input$distrID,
                    type = "Fundamental Uncertainty",
                    xValsSim = xValsForSim(),
                    paramTex = paramTexLookup(input$assumedDistrID),
                    metaParamTex = paramTexLookup(input$assumedDistrID, meta = T),
                )})
                
                # Simulate in stages. First get the base parameters (betas). Then get
                # the intermediate parameters (pi, mu, lambda etc)
                # Then draw ys as appropriate. 
                # For non-covariate distrs, some of these steps are trivial
                paramTilde(paramTildeCreator(paramHat = MLEVars()$paramHat,
                                             paramVCov =  MLEVars()$paramVCov,
                                             1000))
                muTilde(muTildeCreator(paramTilde(),
                                       transformSwitcher(input$assumedDistrID),
                                       xValsForSim()))
                yTilde(yTildeCreator(muTilde(),
                                     model = modelSwitcher(input$assumedDistrID)))
                
                # create outputs for relevant QOI
                QOIOutputs(QOIVisualization(yTilde(), muTilde(), input$assumedDistrID, input$QOIid))
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


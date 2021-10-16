
source("preamble.R")

#######################################################################



server <- function(input, output, session) {
    session$allowReconnect("force") # this will stop it going grey, we hope
    shinyjs::addClass(id = "tabs", class = "navbar-right")
    
    # title text
    titleText <- reactiveVal("")
    titleTextAssumed <- reactiveVal(div(icon("chevron-right"),  tags$b("Model: ---")))
    output$distrNameOutput <- renderUI({titleText()})
    output$assumedDistrNameOutput <- renderUI({titleTextAssumed()})
    
    # creates dynamic tab names and other setup
    
    distrConfig <- reactiveVal()
    assumedDistrConfig <- reactiveVal()
    realDataConfig <- reactiveVal()
    realDataset <- reactiveVal()
    
    observeEvent({input$distrID},{
        
        titleText(div(tags$b("DGP: "),input$distrID))
        assumedDistrSelect(assumedDistrSwitcher(input$distrID))
        if(groupSwitcher(input$distrID) == "Real"){
            realDataConfig(dataConfigSwitcher(input$distrID))
            realDataset( eval(parse(text = realDataConfig()$dataName)))}
        
        distrConfig(distrConfigSwitcher(input$distrID))
        output$dataHeader <- renderUI({dataHeaderFun(distrConfig()$distrGroups)})
        # reset UI elements
        output$xChoiceDiv  <- renderUI({xChoiceDivFun(
            choices = defaultXChoices[1:(distrConfig()$nCovar-1)], hidden=T)})
        output$distPlot <- renderPlot({distrPlotVal()}, height = 1, width = 1)
        output$probHistPlot <- renderPlot({element_blank()}, height = 1, width = 1)
        output$functionalFormPlot <- renderPlot({element_blank()}, height = 1, width = 1)
        output$realDataTable <- renderDataTable({tibble()})
        
    })
    
    observeEvent({input$assumedDistrID},{
        
        assumedDistrConfig(distrConfigSwitcher(input$assumedDistrID))
        titleTextAssumed(div(icon("chevron-right"), tags$b("Model: "),input$assumedDistrID))
        statModelTex(latexSwitcher(input$assumedDistrID, nParamLL = assumedDistrConfig()$nVar, type = "Model"))
        likelihoodTex(latexSwitcher(input$assumedDistrID, type = "Likelihood"))
        output$paramByHandSlider <- renderUI({paramSwitcher(input$assumedDistrID, type = "byHand")})
        
        marginalChoices(marginalsChoicesSwitcher(input$assumedDistrID))
        
        output$marginalSelectorLL <- renderUI({
            if(is.null(input$assumedDistrID)){
                div()
            } else if (nVarSwitcher(input$assumedDistrID) > 1){
                marginalSelectInput(choicesInput = marginalChoices(),
                                    inputID = "marginalSelectedLL")
            } else{
                marginalSelectInput(choicesInput = marginalChoices(),
                                    inputID = "marginalSelectedLL",
                                    hidden = T)}
        })
        
        
        # create n-1 sliders for sim page since x0 is constant
        output$simSliders <- renderUI({
            simMultiSliderFunction(nCovarSwitcher(input$assumedDistrID)-1)})
    })
    
    
    # sliders for top of 1st page
    output$paramSlider <- renderUI({
        if(groupSwitcher(input$distrID) != "Real"){paramSwitcher(input$distrID, type = "param")
        } else  div() })
    output$obsSlider <- renderUI({
        if(groupSwitcher(input$distrID) != "Real"){obsSliderFun(nVarSwitcher(input$distrID)) 
        } else div() })
    # output$obsHeader <- renderUI({obsHeaderFun(nVarSwitcher(input$distrID))})
    
    # choices of assumption depend on actual
    assumedDistrSelect <- reactiveVal()
    output$assumedDistrSelect <- renderUI({assumedDistrSelect()})
    
    # printed data - shouldn't be visible
    outTextP <- reactiveVal("!-----No Data Generated-----!")
    outTextL <- reactiveVal("!-----Generate Data on DGP Page-----!")
    output$outcomeDisplayP <- renderText({outTextP()})
    output$outcomeDisplayL  <- renderText({outTextL()})
    
    
    ## Sets up default choices for X for 1st and 2nd pages
    
    output$marginalSelectorP <- renderUI({
        if(is.null(input$distrID)){
            div()
        } else if ((nVarSwitcher(input$distrID) > 1) &&
                   (distrConfig()$distrGroups != "Real")){
            marginalSelectInput(choicesInput = 1:(nCovarSwitcher(input$distrID) -1),
                                inputID = "marginalSelectedP",
                                includeBetas = F)
        } else{
            marginalSelectInput(choicesInput = 1:(nCovarSwitcher(input$distrID) -1),
                                inputID = "marginalSelectedP",
                                includeBetas = F,
                                hidden = T)}
    })
    
    output$assumedXChoiceDiv  <- renderUI({
        if(length(input$assumedDistrID)==0){div()
        } else if(is.null(input$assumedDistrID)){div()
        } else if (assumedDistrConfig()$nVar > 1){xChoiceDivFun(
            choices = defaultXChoices[1:(assumedDistrConfig()$nCovar-1)], assumed=T)} else{
                xChoiceDivFun(
                    choices = defaultXChoices[1:(assumedDistrConfig()$nCovar-1)], assumed=T, hidden = T)}})
    
    
    # TeX for MLE page
    statModelTex <- reactiveVal("-----")
    likelihoodTex <- reactiveVal("-----")
    output$statModel <- renderUI({statModelTex()})
    output$likelihood <- renderUI({likelihoodTex()})
    
    # dynamic set of choices for Sim page
    output$pickQOIBox <- renderUI({QOISwitcher(input$assumedDistrID)})
    
    # set up some reactives #TODO: do I need to do this
    paramsToUse <- reactiveVal(c())
    paramsTransformed <- reactiveVal()
    marginalChoices <- reactiveVal(c("1"))
    margNumTop <- reactiveVal()
    MLEVars <- reactiveVal(list())
    MLEPlot <- reactiveVal()
    yTilde <- reactiveVal()
    paramTilde <- reactiveVal()
    muTilde <- reactiveVal() #TODO: rename. this is the vector of "final parameters" eg. Xb
    outcomeData <- reactiveVal()
    QOIOutputs <- reactiveVal()
    MLEXBounded <- reactiveVal()
    xValsForSim <- reactiveVal(c())
    distrPlotVal <- reactiveVal(ggplot()+theme_void())
    
    ################################
    # Distribution and setup
    ################################
    
    # TODO BIG: remove observeEvent logic and rely on reactive flow (draw diagram)
    observeEvent({
        input$distrID
        input$param1
        input$param2
        input$param3
        input$param4
        input$nObs
        input$xChoice1
        input$xChoice2
        input$xChoice3
        input$xChoice4
        input$marginalSelectedP
    },{
        if({groupSwitcher(input$distrID)} != "Real"){
            
            if(!is.null(input$param1) &&
               !is.null(eval(parse(text= paste0("input$param",(nVarSwitcher(input$distrID)))) )
               )){
                # browser()
                # creates an object paramsToUse out of however many params there are
                # TODO: find out if there's a better way without NSE
                paramsToUse <- reactiveVal(c())
                listParser(nVarSwitcher(input$distrID),
                           "paramsToUse( c(paramsToUse(), input$param?))", environment())
                
                xChoices <- reactiveVal(c())
                listParser(nCovarSwitcher(input$distrID) - 1,
                           "xChoices( c(xChoices(), input$xChoice?))", environment())
                # x choices
                output$xChoiceDiv   <- renderUI({
                    if(nVarSwitcher(input$distrID) > 1){
                        xChoiceDivFun(choices = xChoices())
                    } else{""}})
                
                # updates Xs based on user choice
                xVals <- if(nVarSwitcher(input$distrID) > 1){
                    reactive({xValGenerator(input$nObs,xChoices())})
                } else reactive({NULL})
                
                
                # create the number of models we'll draw from. For non-covariates, they're all the same
                
                paramsTransformedRaw <- reactive({
                    sapply(1:input$nObs,
                           function(i){transformSwitcher(input$distrID)(paramsToUse(), xVals()[i,])})  })
                
                #TODO: get the damn orientation right by bullying sapply
                if(groupSwitcher(input$distrID) != "Real" && !is.null(dim(paramsTransformedRaw()))){
                    paramsTransformed <- reactive({paramsTransformedRaw() %>%  t()})
                } else {paramsTransformed <- reactive({paramsTransformedRaw()}) }
                # density/mass TeX
                output$distrTex <- renderUI({
                    latexSwitcher(input$distrID,
                                  type = "Distr", paramValsPDF = paramsToUse(), nObs = input$nObs )})
                
                distrPlotVal(try({
                    distrPlot(distrID = input$distrID,
                              paramsTransformed() %>%  as.matrix(),
                              analyticDomainSwitcher(input$distrID),
                              analyticRangeSwitcher(input$distrID))
                }, silent = F))
                
                # analytical distr plot
                output$distPlot <- renderPlot({distrPlotVal()}, height = 350, width = 350)
                
                # histogram if covariates
                output$probHistPlot <- if(nVarSwitcher(input$distrID) > 1){
                    renderPlot({
                        histogramMaker((paramsTransformed() %>%  as.matrix())[,1],
                                       paste0("Parameter $",
                                              paramTexLookup(input$distrID, meta = T), "$"))},
                        height = 350, width = 350)
                } else {renderPlot({element_blank()}, height = 1, width = 1)}
                
                # TODO: a better version of this?
                testVals <- round(rnorm(1, 2),5)
                if(transformSwitcher(input$distrID)(testVals, xVals()) != testVals){
                    output$functionalFormPlot <- renderPlot({functionalFormPlot(
                        transformFun = transformSwitcher(input$distrID),
                        paramRange = chartDomainSwitcher(input$distrID)(distrConfig()$nCovar)[[1]],
                        paramTex = paramTexLookup(input$distrID, meta = F),
                        metaParamTex = paramTexLookup(input$distrID, meta = T),
                        fixValues = paramsToUse(),
                        multi = (nVarSwitcher(input$distrID) != 1),
                        margNum = input$marginalSelectedP %>%  as.numeric(),
                        xVals = xVals(),
                        funcRange = eval(parse(text=distrConfig()$funcFormRange)))},
                        height = 350, width = 350)
                } else {
                    output$functionalFormPlot  <- renderPlot({element_blank()}, height = 1, width = 1)
                }
                output$realDataTable <- renderDataTable({tibble()})
            }
        } else {
            paramsTransformed <- reactiveVal()
            distrPlotVal(ggplot()+theme_void())
            
            output$functionalFormPlot <- renderPlot({element_blank()}, height = 1, width = 1)
            output$distrTex <- renderUI({div()})
            # output$xChoiceDiv <- renderUI({div()})
            output$distPlot <- renderPlot({distrPlotVal()}, height = 1, width = 1)
            output$probHistPlot <- renderPlot({element_blank()}, height = 1, width = 1)
            output$realDataTable <- renderDataTable({
                DT::datatable(
                    realDataSummaryTable(realDataset(),
                                         realDataConfig()$maincol,
                                         realDataConfig()$colnames,
                                         realDataConfig()$datadesc),
                    rownames = FALSE,
                    options = list(paging = FALSE, searching = FALSE, dom = "t"),
                    class = 'order-column cell-border hover',
                )
                
            })
            
        } 
        # generate and print Y
        outcomeData(drawSwitcher(input$distrID, param = paramsTransformed(), nObs = input$nObs))
        outTextP(dataPrintSwitcher(input$distrID, "",outcomeData(), 200))
        outTextL(dataPrintSwitcher(input$distrID, "",outcomeData(), 200))
        
    })
    
    ################################
    # MLE UI and calculation
    ################################
    
    
    
    observeEvent({
        input$distrID
        input$assumedDistrID
        input$param1
        input$param2
        input$param3
        input$param4
        input$byHand1
        input$byHand2
        input$byHand3
        input$byHand4
        input$nObs
        input$xChoice1
        input$xChoice2
        input$xChoice3
        input$xChoice4
        input$assumedXChoice1
        input$assumedXChoice2
        input$assumedXChoice3
        input$assumedXChoice4
        input$marginalSelectedLL
    },{
        if(!is.null(input$param1) || distrConfig()$distrGroups == "Real"){ 
            
            ################################
            # MLE by hand
            ################################
            
            byHandParamsToUse <- reactiveVal(c())
            listParser(nVarSwitcher(input$assumedDistrID),
                       "byHandParamsToUse( c(byHandParamsToUse(), input$byHand?))", environment())
            
            assumedXChoices <- reactiveVal(c())
            listParser(nVarSwitcher(input$distrID),
                       "assumedXChoices( c(assumedXChoices(), input$assumedXChoice?))", environment())
            
            xValsAssumed <- reactive({xValGenerator(length(outcomeData()), assumedXChoices())})
            
            # create the number of models we'll draw from. For non-covariates, they're all the same
            byHandTransformedRaw <- reactive({
                sapply(1:length(outcomeData()),
                       function(i){
                           transformSwitcher(input$assumedDistrID)(
                               byHandParamsToUse(), xValsAssumed()[i,])})  })
            
            # todo get the damn orientation right by bullying sapply
            if(!is.null(dim(byHandTransformedRaw()))){
                byHandTransformed <- reactive({byHandTransformedRaw() %>%  t()})
            } else {byHandTransformed <- reactive({byHandTransformedRaw()}) }
            
            output$MLEByHandPlot <- renderPlot({
                handMLESwitcher(input$assumedDistrID,
                                data = outcomeData(), 
                                domain = analyticDomainSwitcher(input$assumedDistrID),
                                range = analyticRangeSwitcher(input$assumedDistrID),
                                pdfFun = pdfSwitcher(input$assumedDistrID),
                                assumedParam = byHandTransformed() %>%  as.matrix(),
                                multiModel = (nVarSwitcher(input$assumedDistrID) != 1)
                )
            }, height = 301)
            
            
            
            ################################
            # MLE regular
            ################################
            
            ## this changes the state that likelihood functions will read. 
            # print new assumed X
            
            
            output$assumedXChoiceDiv   <- renderUI({
                if(nVarSwitcher(input$assumedDistrID) > 1){
                    xChoiceDivFun(choices = assumedXChoices(), assumed = T)
                } else{""}})
            
            
            
            output$marginalSelectorLL <- renderUI({
                if(nVarSwitcher(input$assumedDistrID) > 1){
                    marginalSelectInput(choicesInput = marginalChoices(),
                                        inputID = "marginalSelectedLL",
                                        currentChoice = input$marginalSelectedLL,
                                        fixedValues = byHandParamsToUse())} else {div()}
            })
            testVals <- round(rnorm(1, 2),5)
            if(transformSwitcher(input$assumedDistrID)(testVals, xValsAssumed()) != testVals){
                output$functionalFormPlotLL <- renderPlot({functionalFormPlot(
                    transformFun = transformSwitcher(input$assumedDistrID),
                    paramRange = chartDomainSwitcher(input$assumedDistrID)(assumedDistrConfig()$nCovar)[[1]],
                    paramTex = paramTexLookup(input$assumedDistrID, meta = F),
                    metaParamTex = paramTexLookup(input$assumedDistrID, meta = T),
                    fixValues = byHandParamsToUse(),
                    multi = (nVarSwitcher(input$assumedDistrID) != 1),
                    margNum = margNumTop(),
                    xVals = xValsAssumed(),
                    funcRange = eval(parse(text=assumedDistrConfig()$funcFormRange)))},
                    height = 350, width = 350)
            } else {
                output$functionalFormPlotLL  <- renderPlot({element_blank()}, height = 1, width = 1)
            }
            
            # profile likelihood choice
            margNumTop(which(marginalsChoicesSwitcher(input$assumedDistrID)== input$marginalSelected2))
            
            # compute MLE variables and make plot
            MLEVars(MLEstimator(outcome = outcomeData(),
                                chartDomain = chartDomainSwitcher(input$assumedDistrID)(assumedDistrConfig()$nCovar),
                                likelihoodFun =  likelihoodFunSwitcher(input$assumedDistrID),
                                paramName = paramNameSwitcher(input$assumedDistrID),
                                margNum = margNumTop(),
                                xVals = xValsAssumed(), 
                                fixValues = byHandParamsToUse(),
            ))
            MLEPlot(MLEVars()$plot)
            output$MLEPlot <- renderPlot({MLEPlot()})
            
            # TODO: merge this nonsense into big TeX
            # outputs of MLE results on p2 and p3
            output$simParamLatex <- renderUI({
                simMLELatex(paste0("\\(\\hat{",
                                   paramTexLookup(input$assumedDistrID),"} =\\) "), MLEVars()$paramHat )})
            output$simVcovLatex <- renderUI({
                simMLELatex(
                    paste0("\\(\\hat{V}(\\hat{",
                           paramTexLookup(input$assumedDistrID),"}) =\\) "), MLEVars()$paramVCov  )})
            
            output$MLEParamLatex <- renderUI({
                simMLELatex(paste0("\\(\\hat{",
                                   paramTexLookup(input$assumedDistrID),"} =\\) "), MLEVars()$paramHat )})
            output$MLEVcovLatex <- renderUI({
                simMLELatex(
                    paste0("\\(\\hat{V}(\\hat{",
                           paramTexLookup(input$assumedDistrID),"}) =\\) "), MLEVars()$paramVCov )})
            
            MLEPlot(MLEVars()$plot)
            paramIndex <- reactive({
                if(length(margNumTop()) >0) {margNumTop()} else {1}
            })
            
            MLEXBounded(max(min(
                byHandParamsToUse()[paramIndex()],
                chartDomainSwitcher(input$assumedDistrID)(assumedDistrConfig()$nCovar)[[paramIndex()]]$to),
                chartDomainSwitcher(input$assumedDistrID)(assumedDistrConfig()$nCovar)[[paramIndex()]]$from))
            MLEPlot(MLEPlot() +
                        annotate("segment",
                                 x = MLEXBounded(),
                                 xend = MLEXBounded(),
                                 y = -Inf, yend = Inf, linetype=2,
                                 color = "#BF5803", alpha = .75, size = 1.5))
            
            
        }
    })
    
    observeEvent({
        input$resetByHand
    }, {
        lapply(1:nVarSwitcher(input$assumedDistrID), function(i){
            updateSliderInput(
                inputId = paste0("byHand",i),
                value = MLEVars()$paramHat[i], session = session
            )
        }) 
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
                    input$assumedDistrID,
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
             options(warn = -1, shiny.fullstacktrace = T)
             onStop(function(){
                 options(warn = oldw)
                 
             })
             
         })


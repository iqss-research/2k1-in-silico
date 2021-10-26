
source("preamble.R")

#######################################################################



server <- function(input, output, session) {
    session$allowReconnect("force") # this will stop it going grey, we hope
    shinyjs::addClass(id = "tabs", class = "navbar-right")
    
    #########
    # Prob
    #########
    output$distrNameOutput <- renderUI({})
    output$paramSlider <- renderUI({})
    output$dataHeader <- renderUI({})
    output$xChoiceDiv  <- renderUI({})
    output$obsSlider <- renderUI({})
    output$marginalSelectorP <- renderUI({})
    output$outcomeDisplayP <- renderText({})
    output$distrTex <- renderUI({})
    output$ffhover_info <- renderUI({})
    
    output$distPlot <- renderPlot({element_blank()}, height = 1, width = 1)
    output$probHistPlot <- renderPlot({element_blank()}, height = 1, width = 1)
    output$functionalFormPlot <- renderPlot({element_blank()}, height = 1, width = 1)
    output$realDataTable <- renderDataTable({tibble()})
    output$specialPlot <-  renderPlot({element_blank()}, height = 1, width = 1)
    
    #########
    # MLE
    #########
    output$assumedDistrNameOutput <- renderUI({})
    output$paramByHandSlider <- renderUI({})
    output$marginalSelectorLL <- renderUI({})
    output$assumedXChoiceDiv <- renderUI({})
    output$assumedDistrSelect  <- renderUI({})
    output$outcomeDisplayL  <- renderText({})
    output$marginalSelectorLL <- renderUI({})
    output$marginalSelectorLLF  <- renderUI({})
    output$statModel <- renderUI({})
    output$likelihood <- renderUI({})
    output$MLEhover_info <- renderUI({}) 
    output$ffLhover_info  <- renderUI({})
    output$MLEParamLatex <- renderUI({})
    output$MLEVcovLatex <- renderUI({})
    
    
    output$MLEByHandPlot <- renderPlot({element_blank()}, height = 1, width = 1)
    output$MLEPlot <- renderPlot({})
    output$functionalFormPlotLL <- renderPlot({element_blank()}, height = 1, width = 1)
    
    
    
    #########
    # Sim
    #########
    
    output$simSliders  <- renderUI({})
    output$marginalSelectorSim <- renderUI({})
    output$pickQOIBox <- renderUI({})
    output$simParamLatex <- renderUI({})
    output$simVcovLatex <- renderUI({})
    output$simEstimationLatex <- renderUI({})
    output$simFundamentalLatex <- renderUI({})
    output$SimHover_info <- renderUI({})
    
    output$functionalFormPlotSim  <- renderPlot({element_blank()}, height = 1, width = 1)
    output$QOIChart  <- renderPlot({element_blank()}, height = 1, width = 1)
    
    
    ############################
    # Probability Tab: Top Part
    ############################
    
    ########### set up #############
    
    
    # store the configuration variables
    distrConfig <- reactive({distrConfigSwitcher(input$distrID)})
    
    # set up all the main user inputs
    output$distrNameOutput <- renderUI({div(tags$b("DGP: "),input$distrID)})
    output$paramSlider <-renderUI({eval(parse(text=paste0(distrConfig()$sliderFun, "inputName = 'param')")))})
    output$obsSlider <- renderUI({if(distrConfig()$distrGroup != "Real"){obsSliderFun(distrConfig()$nVar)} else div() })
    output$xChoiceDiv  <- renderUI({
        if(distrConfig()$nCovar > 1 ){
            xChoiceDivFun(choices = defaultXChoices[1:(distrConfig()$nCovar-1)])
        } else{xChoiceDivFun(hidden=T)}})
    
    # get the input parameters
    output$distrTex <- renderUI({
        latexSwitcher(input$distrID, type = "Distr", nParamPDF = distrConfig()$nVar,
                      nXValsPDF = distrConfig()$nCovar-1)})
    ########### begin computations #############
    paramsToUse <- reactive({
        vec <- input$param1
        if(!is.null(input$param2)){vec <- c(vec, input$param2)}
        if(!is.null(input$param3)){vec <- c(vec, input$param3)}
        if(!is.null(input$param4)){vec <- c(vec, input$param4)}
        if(!is.null(input$param5)){vec <- c(vec, input$param5)}
        vec[1:distrConfig()$nVar]
    })
    
    xChoices <- reactive({
        vec <- c()
        if(!is.null(input$xChoice1)){vec <- c(vec, input$xChoice1)}
        if(!is.null(input$xChoice2)){vec <- c(vec, input$xChoice2)}
        if(!is.null(input$xChoice3)){vec <- c(vec, input$xChoice3)}
        if(!is.null(input$xChoice4)){vec <- c(vec, input$xChoice4)}
        vec[1:(distrConfig()$nCovar-1)]
    })
    
    xVals <- reactive({
        req(xChoices())
        tryCatch({
            if(distrConfig()$nCovar > 1){xValGenerator(input$nObs,xChoices())} else {NULL} 
        }, error = function(e){NULL})
    })
    
    
    paramsTransformed <- reactive({
        req(paramsToUse())
        if(distrConfig()$nCovar > 1) {req(xVals())}
        vec <- sapply(1:input$nObs,
                      function(i){(parser(distrConfig()$transformFun))(paramsToUse(), xVals()[i,])})  
        if(!is.null(dim(vec))){vec %>%  t()} else {vec} ##TODO: figure out how to use apply to avoid
        
    })
    
    output$distPlot <- renderPlot({
        # browser()
        distrPlot(distrID = input$distrID,
                  paramsTransformed() %>%  as.matrix(),
                  parser(distrConfig()$analyticDomain),
                  parser(distrConfig()$analyticRange))},
        height = 350, width = 350)
    

    output$marginalSelectorP <- renderUI({
        marginalSelectInput(choicesInput = paste0("X",1:(distrConfig()$nCovar-1)),
                            inputID = "marginalSelectedP", 
                            hidden = (distrConfig()$nVar == 1)) # hide for univariates
    })
    
    observeEvent({input$distrID},{
        output$probHistPlot <- renderPlot({
            if(distrConfig()$nVar > 1){ 
                histogramMaker((paramsTransformed() %>%  as.matrix())[,1],
                               paste0("$",distrConfig()$intrParamTex, "$"))
            } else{element_blank()}},
            height = if(distrConfig()$nVar > 1){350} else {1},
            width = if(distrConfig()$nVar > 1){350} else {1})
        
        output$specialPlot <- if(distrConfig()$distrGroup == "Ordered" ){
            renderPlot({orderedDistSpecialPlot(distrConfig()$yStarPDF,
                                               paramsTransformed())},
                       height = 350, width = 350)
        } else {renderPlot({element_blank()}, height = 1, width = 1)}
        
    })
    
    observeEvent({input$marginalSelectedP},{
        testVals <- round(rnorm(1, 2),5)
        if((parser(distrConfig()$transformFun))(testVals, xVals()) != testVals){
            output$functionalFormPlot <- renderPlot({functionalFormPlotSwitcher(
                input$distrID,
                transformFun = parser(distrConfig()$transformFun),
                paramRange = parser(distrConfig()$chartDomain)(distrConfig()$nCovar)[[1]],
                paramTex = distrConfig()$paramTex,
                metaParamTex = distrConfig()$intrParamTex,
                fixValues = paramsToUse(),
                multi = (distrConfig()$nVar != 1),
                margNum = substr(input$marginalSelectedP,2,2) %>%  as.numeric(),
                xVals = xVals(),
                xChoice = xChoices(),
                funcRange = parser(distrConfig()$funcFormRange),
                pdfFun = parser(distrConfig()$pdfList)(input$distrID))},
                height = 350, width = 350)
            
            #TODO: how can this call be shorter tho

            output$ffhover_info <- renderUI({
                if(distrConfig()$nCovar > 1){
                    tooltipFun(input$ffplot_hover, "Other X fixed at means, parameters at chosen values")}else {div()}  })

        } else {
            output$functionalFormPlot  <- renderPlot({element_blank()}, height = 1, width = 1)
            output$ffhover_info <- renderUI({div()})
        }
        
    })
    output$dataHeader <- renderUI({dataHeaderFun(distrConfig()$distrGroup)})  
    
    outcomeData <- reactive({(parser(distrConfig()$drawFun))(param = paramsTransformed(), nObs = input$nObs)})
    output$outcomeDisplayP <- renderText({dataPrintHelper(outcomeData(), 200)})
    
    
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

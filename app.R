
source("preamble.R")

#######################################################################



server <- function(input, output, session) {
  session$allowReconnect("force") # this will stop it going grey, we hope
  shinyjs::addClass(id = "tabs", class = "navbar-right")
  
  ############################
  # Probability Tab
  ############################
  
  ########### set up and UI #############
  
  # store the configuration variables
  distrConfig <- reactive({distrConfigSwitcher(input$distrID)})
  
  observeEvent(input$distrID, {
    output$functionalFormPlot  <- renderPlot({element_blank()}, height = 1, width = 1)
  })
  
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
  
  output$marginalSelectorP <- renderUI({
    marginalSelectInput(choicesInput = paste0("X",1:(distrConfig()$nCovar-1)),
                        inputID = "marginalSelectedP", 
                        hidden = (distrConfig()$nVar == 1)) # hide for univariates
  })
  
  
  ########### probability page computations #############
  probParams <- reactive({
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
    req(probParams())
    if(distrConfig()$nCovar > 1) {req(xVals())}
    vec <- sapply(1:input$nObs,
                  function(i){(parser(distrConfig()$transformFun))(probParams(), xVals()[i,])})  
    if(!is.null(dim(vec))){vec %>%  t()} else {vec} ##TODO: figure out how to use apply to avoid
    
  })
  ########### RHS plots #############
  output$distPlot <- renderPlot({
    req(paramsTransformed())
    parser(distrConfig()$distrPlot)(
      paramsTransformed() %>%  as.matrix(),
      parser(distrConfig()$analyticDomain),
      parser(distrConfig()$analyticRange))},
    height = 350, width = 350)
  
  
  observeEvent({input$distrID},{
    
    output$probHistPlot <- renderPlot({
      req(paramsTransformed())
      if(distrConfig()$nVar > 1){ 
        histogramMaker((paramsTransformed() %>%  as.matrix())[,1],
                       paste0("$",distrConfig()$intrParamTex, "$"))
      } else{element_blank()}},
      height = if(distrConfig()$nVar > 1){350} else {1},
      width = if(distrConfig()$nVar > 1){350} else {1})
    
    output$specialPlot <- if(distrConfig()$distrGroup == "Ordered" ){
      renderPlot({
        req(paramsTransformed())
        orderedDistSpecialPlot(parser(distrConfig()$yStarPDF),paramsTransformed())},
        height = 350, width = 350)
    } else {renderPlot({element_blank()}, height = 1, width = 1)}
    
  })
  
  observeEvent({input$distrID},{
    req(probParams())
    if(distrConfig()$nCovar > 1) {req(xVals())}
    testVals <- round(rnorm(1, 2),5)
    if((parser(distrConfig()$transformFun))(testVals, xVals()) != testVals){
      margNumFFP <- substr(input$marginalSelectedP,2,2) %>%  as.numeric()
      output$functionalFormPlot <- renderPlot({functionalFormPlotSwitcher(
        input$distrID,
        transformFun = parser(distrConfig()$transformFun),
        paramRange = parser(distrConfig()$chartDomain)(distrConfig()$nCovar)[[1]],
        paramTex = parser(distrConfig()$paramList)[[margNumFFP]],
        metaParamTex = distrConfig()$intrParamTex,
        fixValues = probParams(),
        multi = (distrConfig()$nVar != 1),
        margNum = margNumFFP,
        xVals = xVals(),
        xChoice = xChoices(),
        funcRange = parser(distrConfig()$funcFormRange),
        pdfFun = parser(distrConfig()$pdfList))},
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
  
  ########### probability tab data gen #############
  output$dataHeader <- renderUI({dataHeaderFun(distrConfig()$distrGroup)})  
  outcomeData <- reactive({(parser(distrConfig()$drawFun))(param = paramsTransformed(), nObs = input$nObs)})
  output$outcomeDisplayP <- renderText({
    req(outcomeData())
    dataPrintHelper(outcomeData(), 200)})
  
  ############################
  # MLE Tab
  ############################
  
  ########### top UI #############
  output$outcomeDisplayL  <- renderText({dataPrintHelper(outcomeData(), 200)})
  
  output$assumedDistrSelect  <- renderUI({
    div(selectInput(
      inputId = "assumedDistrID",
      label = tags$p(tags$b("Assumed Distribution"),style = "font-size:15px; !important"),
      choices = parser(distrConfig()$assumedDistrChoices), 
      width = "250px"), class = "distrInput")
    
  })
  
  assumedDistrConfig <- reactive({
    req(input$assumedDistrID)
    distrConfigSwitcher(input$assumedDistrID)})
  
  
  ########### tab title #############
  titleTextAssumed <- reactiveVal()
  observeEvent(input$distrID, titleTextAssumed(div(icon("chevron-right"),  tags$b("Model: ---"))))
  observeEvent(input$assumedDistrID,titleTextAssumed(div(icon("chevron-right"), tags$b("Model: "),input$assumedDistrID)))
  output$assumedDistrNameOutput <- renderUI({titleTextAssumed()})
  
  ########### LHS UI #############
  output$assumedXChoiceDiv <- renderUI({
    if (assumedDistrConfig()$nVar > 1){xChoiceDivFun(
      choices = if(input$assumedDistrID == input$distrID) {xChoices()
      } else {defaultXChoices[1:(assumedDistrConfig()$nCovar-1)]}, assumed=T)}
    else {xChoiceDivFun(hidden = T)}})
  
  output$statModel <- renderUI({
    req(input$assumedDistrID)
    latexSwitcher(input$assumedDistrID, nXValsAssumed = assumedDistrConfig()$nCovar-1,
                  nParamLL = assumedDistrConfig()$nVar, type = "Model")})
  output$likelihood <- renderUI({
    req(input$assumedDistrID)
    latexSwitcher(input$assumedDistrID, type = "Likelihood")})
  
  ########### RHS UI #############
  output$paramByHandSlider <- renderUI({eval(parse(text=paste0(assumedDistrConfig()$sliderFun, "inputName = 'byHand')")))})
  
  output$marginalSelectorLL <- renderUI({
    if (assumedDistrConfig()$nVar > 1){
      marginalSelectInput(choicesInput = parser(assumedDistrConfig()$marginalsChoicesList),
                          inputID = "marginalSelectedLL")
    } else{marginalSelectInput(hidden = T)}})
  
  output$marginalSelectorLLF   <- renderUI({
    if (assumedDistrConfig()$nVar > 1){
      marginalSelectInput(choicesInput = paste0("X",1:(assumedDistrConfig()$nCovar-1)),
                          inputID = "marginalSelectedLLF")
    } else{marginalSelectInput(hidden = T)}})
  
  
  ########### Computations #############
  #TODO: this feels redundant... modules?
  byHandParams <- reactive({
    vec <- input$byHand1
    if(!is.null(input$byHand2)){vec <- c(vec, input$byHand2)}
    if(!is.null(input$byHand3)){vec <- c(vec, input$byHand3)}
    if(!is.null(input$byHand4)){vec <- c(vec, input$byHand4)}
    if(!is.null(input$byHand5)){vec <- c(vec, input$byHand5)}
    vec[1:assumedDistrConfig()$nVar]
  })
  
  assumedXChoices <- reactive({
    vec <- c()
    if(!is.null(input$assumedXChoice1)){vec <- c(vec, input$assumedXChoice1)}
    if(!is.null(input$assumedXChoice2)){vec <- c(vec, input$assumedXChoice2)}
    if(!is.null(input$assumedXChoice3)){vec <- c(vec, input$assumedXChoice3)}
    if(!is.null(input$assumedXChoice4)){vec <- c(vec, input$assumedXChoice4)}
    vec[1:(assumedDistrConfig()$nCovar-1)]
  })
  
  assumedXVals <- reactive({
    req(assumedXChoices())
    tryCatch({
      if(assumedDistrConfig()$nCovar > 1){xValGenerator(length(outcomeData()),assumedXChoices())} else {NULL} 
    }, error = function(e){NULL})
  })
  
  
  byHandTransformed <- reactive({
    req(byHandParams())
    if(assumedDistrConfig()$nCovar > 1) {req(assumedXVals())}
    vec <- sapply(1:length(outcomeData()),
                  function(i){(parser(assumedDistrConfig()$transformFun))(byHandParams(), assumedXVals()[i,])})  
    if(!is.null(dim(vec))){vec %>%  t()} else {vec} ##TODO: figure out how to use apply to avoid
    
  })
  
  margNumLL <- eventReactive({input$assumedDistrID},{
    tmp <- which(parser(assumedDistrConfig()$marginalsChoicesList) == input$marginalSelectedLL)
    if(length(tmp) == 0){1} else{tmp}
  })
  
  MLEResult <- reactive({
    req(outcomeData())
    if(assumedDistrConfig()$nCovar > 1) {req(margNumLL(), assumedXVals())}
    likelihoodEstimateFun(
      chartDomain = parser(assumedDistrConfig()$chartDomain)(assumedDistrConfig()$nVar),
      likelihoodFun = parser(assumedDistrConfig()$likelihoodFun),
      margNum = margNumLL(),
      outcome = outcomeData(),
      xVals = assumedXVals(),
      optimMethod = assumedDistrConfig()$optimMethod)})
  
  
  # reset to MLE button
  observeEvent(input$resetByHand, {
    req(MLEResult())
    lapply(1:assumedDistrConfig()$nVar, function(i){
      updateSliderInput(inputId = paste0("byHand",i),
                        value = MLEResult()$paramHat[i], session = session)}) })
  
  
  ########### Plots #############
  output$MLEByHandPlot <- renderPlot({
    req(byHandTransformed())
    handMLESwitcher(
      input$assumedDistrID,
      data = outcomeData(),
      domain = parser(assumedDistrConfig()$analyticDomain),
      range = parser(assumedDistrConfig()$analyticRange),
      pdfFun = parser(assumedDistrConfig()$pdfList),
      assumedParam = byHandTransformed() %>%  as.matrix(),
      multiModel = (assumedDistrConfig()$nVar != 1))}, height = 301)
  
  # only add q approx if we're at MLE
  MLEPlot <- reactive({
    req(MLEResult(), margNumLL())
    MLEPlotFun(MLEResult(), parser(assumedDistrConfig()$paramList)[[margNumLL()]])
  })
  
  MLEXBounded <- reactive({max(min(
    byHandParams()[margNumLL()],
    parser(assumedDistrConfig()$chartDomain)(assumedDistrConfig()$nCovar)[[margNumLL()]]$to),
    parser(assumedDistrConfig()$chartDomain)(assumedDistrConfig()$nCovar)[[margNumLL()]]$from)})
  
  
  output$MLEPlot <- renderPlot({
    MLEPlot() + annotate("segment",
                         x = MLEXBounded(),
                         xend = MLEXBounded(),
                         y = -Inf, yend = Inf, linetype=2,
                         color = baseColor2, alpha = .75, size = 1.5)
  })
  output$MLEhover_info <-  renderUI({if(assumedDistrConfig()$nCovar > 1){
    tooltipFun(input$MLEplot_hover, "Other parameters fixed at MLEs")} else {div()} })
  
  
  observeEvent({
    input$assumedDistrID
    MLEResult()
  },{
    testVals <- round(rnorm(1, 2),5)
    if((parser(assumedDistrConfig()$transformFun))(testVals, assumedXVals()) != testVals){
      margNumLLF <- substr(input$marginalSelectedLLF,2,2) %>%  as.numeric()
      if(length(margNumLLF) == 0){margNumLLF <- 1}
      
      output$functionalFormPlotLL <- renderPlot({functionalFormPlotSwitcher(
        input$assumedDistrID,
        transformFun = parser(assumedDistrConfig()$transformFun),
        paramRange = parser(assumedDistrConfig()$chartDomain)(assumedDistrConfig()$nCovar)[[1]],
        paramTex = parser(assumedDistrConfig()$paramList)[[margNumLLF]],
        metaParamTex = assumedDistrConfig()$intrParamTex,
        fixValues = byHandParams(),
        multi = (assumedDistrConfig()$nVar != 1),
        margNum = margNumLLF,
        xVals = assumedXVals(),
        xChoice = assumedXChoices(),
        funcRange = parser(assumedDistrConfig()$funcFormRange),
        pdfFun = parser(assumedDistrConfig()$pdfList))},
        height = 350, width = 350)
      
      #TODO: how can this call be shorter tho
      
      output$ffLhover_info <- renderUI({
        if(distrConfig()$nCovar > 1){
          tooltipFun(input$ffLplot_hover, "Other X fixed at means, parameters at MLEs")}else {div()}  })
      
    } else {
      output$functionalFormPlot  <- renderPlot({element_blank()}, height = 1, width = 1)
      output$ffLhover_info <- renderUI({div()})
    }
  })
  
  output$MLEParamLatex <- renderUI({
    req(MLEResult())
    coeffLatex(parser(assumedDistrConfig()$paramList), MLEResult()$paramHat )})
  output$MLEVcovLatex <- renderUI({
    req(MLEResult())
    vCovLatex(parser(assumedDistrConfig()$paramList), MLEResult()$paramVCov )})
  
  
  #########
  # Sim
  #########
  
  output$simHeader <- renderUI({
    if(is.null(input$assumedDistrID)){ tags$b("Please Choose A Model", style = "color:red")
    } else { tags$p(tags$b("From Likelihood Tab"))} 
  })
  
  output$simParamLatex <- renderUI({
    req(MLEResult())
    coeffLatex(parser(assumedDistrConfig()$paramList), MLEResult()$paramHat )})
  output$simVcovLatex <- renderUI({
    req(MLEResult())
    vCovLatex(parser(assumedDistrConfig()$paramList), MLEResult()$paramVCov )})
  
  output$simSliders  <-  renderUI({
    req(input$assumedDistrID)
    simMultiSliderFunction(assumedDistrConfig()$nCovar-1)})
  output$simEstimationLatex <-  renderUI({
    req(input$assumedDistrID)
    latexSwitcher(
      input$assumedDistrID,
      type = "Estimation Uncertainty",
      paramTex = assumedDistrConfig()$paramTex
    )})
  
  simXVals <- reactive({
    vec <- c()
    if(!is.null(input$simX1)){vec <- c(vec, input$simX1)}
    if(!is.null(input$simX2)){vec <- c(vec, input$simX2)}
    if(!is.null(input$simX3)){vec <- c(vec, input$simX3)}
    if(!is.null(input$simX4)){vec <- c(vec, input$simX4)}
    vec[1:(assumedDistrConfig()$nCovar-1)]
  })
  
  output$simFundamentalLatex <-  renderUI({
    req(simXVals())
    latexSwitcher(
      input$assumedDistrID,
      type = "Fundamental Uncertainty",
      xValsSim = simXVals(),
      paramTex = assumedDistrConfig()$paramTex,
      intrParamTex = assumedDistrConfig()$intrParamTex,
    )})
  
  output$pickQOIBox <- renderUI({
    req(input$assumedDistrID)
    QOISwitcher(input$assumedDistrID)})
  
  # Simulate in stages. First get the base parameters (betas). Then get
  # the intermediate parameters (pi, mu, lambda etc)
  # Then draw ys as appropriate. 
  # For non-covariate distrs, some of these steps are trivial
  paramTilde <- reactive({paramTildeCreator(
    paramHat = MLEResult()$paramHat,paramVCov =  MLEResult()$paramVCov, 1000)})
  intrTilde <- reactive({
    req(simXVals(), paramTilde())
    intrTildeCreator(paramTilde(),parser(assumedDistrConfig()$transformFun), simXVals()) })
  yTilde<- reactive({
    req(intrTilde())
    yTildeCreator(intrTilde(),model = parser(assumedDistrConfig()$drawFun))})
  
  output$marginalSelectorSim <- renderUI({
    if (assumedDistrConfig()$nVar > 1){
      marginalSelectInput(choicesInput = paste0("X",1:(assumedDistrConfig()$nCovar-1)),
                          inputID = "marginalSelectedSim")
    } else{marginalSelectInput(hidden = T)}})
  
  
  eventReactive({input$assumedDistrID},{
    testVals <- round(rnorm(1, 2),5)
    if(((parser(assumedDistrConfig()$transformFun))(testVals, assumedXVals()) != testVals) &
       (assumedDistrConfig()$distrGroup != "Ordered" )){
      output$functionalFormPlotSim <- renderPlot({
        functionalFormWithCI(transformFun = parser(assumedDistrConfig()$transformFun),
                             fixValuesX = simXVals(),
                             paramTildes = paramTilde(),
                             funcRange = parser(assumedDistrConfig()$funcFormRange),
                             margNum = substr(input$marginalSelectedSim,2,2) %>%  as.numeric(),
                             metaParamTex = paramTexLookup(input$assumedDistrID, meta = T) )}, height = 350)
      
      output$SimHover_info <- renderUI({
        if(assumedDistrConfig()$nCovar > 1){
          tooltipFun(input$SimPlot_hover, "Other X fixed at chosen values")} else {div()}  })
      
    } else {output$functionalFormPlotSim <-  renderPlot({element_blank()}, height = 1, width = 1)}
    
  })
  
  QOIOutputs <- reactive({
    req(yTilde(), simXVals())
    QOIVisualization(yTilde(), intrTilde(), assumedDistrConfig(), input$QOIid)})
  
  output$QOIChart  <- renderPlot({
    req(QOIOutputs())})
  
  
  
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

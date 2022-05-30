#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ###########################
  session$allowReconnect("force") # this will stop it going grey, we hope
  # shinyjs::addClass(id = "tabs", class = "navbar-right")
  # a greyout chrome hack
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })

  ###########################
  # loading
  ###########################

  # load tooltips
  # load distributions
  distrDF <- readxl::read_excel(app_sys("DistrNames.xlsx"),1)
  QOIDF <- readxl::read_excel(app_sys("QOIList.xlsx"),1)
  QOIChoices <- QOIDF$Name

  optGroups <- list()
  for(g in unique(distrDF$distrGroup)){

    distrs <- distrDF$distrList[which(distrDF$distrGroup == g)]

    newNames <- c(names(optGroups), g)
    optGroups <- append(optGroups, list(distrs))
    names(optGroups) <- newNames

  }

  xGenerationChoicesShort <- substr(
    xGenerationChoices, 0, stringr::str_length(xGenerationChoices)-2)

  xGroups <- list()

  for(g in c("Bernoulli", "Uniform", "Normal", "Poisson")){

    newXs <- xGenerationChoices[which(xGenerationChoicesShort == g)]

    newNames <- c(names(xGroups), g)
    xGroups <- append(xGroups, list(newXs))
    names(xGroups) <- newNames

  }

  ############################
  # Intro and help
  ############################

  output$introductoryText <- renderUI({
    HTML((tutorialText %>%  dplyr::filter(Name == "Intro"))$content)

  })


  observe({
    shinyjs::onclick("shield", updateTabsetPanel(session, "tabs", selected = "Introduction"))
  })


  ############################
  # Tab Titles
  ############################

  output$distrNameOutput <- renderUI({
    div(id = "DGPTitle", tags$b("DGP: "),input$distrID,
        helperMakerNavbar(str = "DGPs and Probability"),
        title = "DGPs/Probability Tab"
    )
  })

  ############################
  # Probability Tab
  ############################

  ########### set up and UI #############


  ###########################
  distrConfig <- reactive({
    req(input$distrID)
    distrDF %>%  dplyr::filter(distrList == input$distrID)
  })

  numX <- reactiveVal(NULL)
  observeEvent(input$distrID, {
    # Reset/invalidate some stuff
    # output$functionalFormPlot  <- renderPlot({ggplot2::element_blank()}, height = 1, width = 1)
    # probParams <- paramsTransformed <- xChoices <- xVals <-  reactive({NULL})

    output$xChoiceDiv  <- renderUI({
      if(distrConfig()$nCovar > 1 ){
        xChoiceDivFun(
          choices = if(!is.null(xChoices())){
            if(sum(!is.na(xChoices())) == (numX()-1)){
              xChoices() } else { c(xChoices()[!is.na(xChoices())], defaultXChoices[numX()-1])  }
          } else {defaultXChoices[1:(numX()-1)]},
          plus = (distrConfig()$nCovar > numX()),
          minus = (numX() > 2)
        )
      } else{xChoiceDivFun(hidden=T)}})

    MLEResult <- reactive({NULL})
    if((distrConfig()$nCovar > 1)){ numX(2) } else {numX(1)}

  })


  # UI elements used/seen everywhere
  output$dgpChoiceUI <- renderUI({
    selectInput(
      inputId = "distrID",
      label = tags$p(
        tags$b("Data Generation Process"),
        style = "font-size:15px; !important"
      ),
      choices = optGroups, selected = selectedDist,
      width = "250px")
  })

  # set up all the main user inputs
  output$obsSlider <- renderUI({
    req(distrConfig())
    obsSliderFun(distrConfig()$nVar) })


  output$paramSlider <-renderUI({
    manyParamSliderMaker(
      minVal = distrConfig()$sliderMin,
      maxVal = distrConfig()$sliderMax,
      startVals = parser(distrConfig()$sliderStarts)[1:(numX() + distrConfig()$nNonXParams)],
      sigmaScale =  parser(distrConfig()$sigmaScale),
      paramTex = distrConfig()$paramTex,
      secondParamTex  = distrConfig()$secondParamTex )
  })

  # button to add X
  observeEvent(input$addXVar, {
    req(numX())
    if(numX() < distrConfig()$nCovar) {
      numX(numX() + 1)
      probParams <- paramsTransformed <- xChoices <- xVals <-  reactive({NULL})
    }
  })

  # button to add X
  observeEvent(input$subtractXVar, {
    req(numX())
    if(numX() > 2) {
      numX(numX() - 1)
      probParams <- paramsTransformed <- xChoices <- xVals <-  reactive({NULL})
    }
  })


  output$distrTex <- renderUI({
    parser(distrConfig()$latexList)(type = "Distr", nXValsPDF = numX()-1) })


  output$marginalSelectorP <- renderUI({
    marginalSelectInput(choicesInput = paste0("X",1:(numX()-1)),
                        inputID = "marginalSelectedP",
                        hidden = (distrConfig()$nVar == 1)) # hide for univariates
  })


  ########### probability page computations #############
  probParams <- reactive({
    vec <- input$param1
    if(!isnothing(input$param2)){vec <- c(vec, input$param2)}
    if(!isnothing(input$param3)){vec <- c(vec, input$param3)}
    if(!isnothing(input$param4)){vec <- c(vec, input$param4)}
    if(!isnothing(input$param5)){vec <- c(vec, input$param5)}
    vec[1:(numX() + distrConfig()$nNonXParams)]
  })

  xChoices <- reactive({
    vec <- c()
    if(!isnothing(input$xChoice1)){vec <- c(vec, input$xChoice1)}
    if(!isnothing(input$xChoice2)){vec <- c(vec, input$xChoice2)}
    if(!isnothing(input$xChoice3)){vec <- c(vec, input$xChoice3)}
    if(!isnothing(input$xChoice4)){vec <- c(vec, input$xChoice4)}
    vec[1:(numX() -1)]
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
                  function(i){(parser(distrConfig()$transformFun))(probParams(), xVals()[i,], DGP = T)})
    if(!is.null(dim(vec))){vec %>%  t()} else {vec} ##TODO: figure out how to use apply to avoid

  })

  ########### RHS plots #############
  output$distPlot <- renderPlot({
    req(distrConfig(),paramsTransformed())
    tryCatch({parser(distrConfig()$distrPlot)(
      paramsTransformed() %>%  as.matrix(),
      parser(distrConfig()$analyticDomain),
      parser(distrConfig()$analyticRange))},
      error = function(e){element_blank()})
  },
  height = 350, width = 350)

  observeEvent({input$distrID},{
    output$probHistPlot <- renderPlot({
      req(paramsTransformed())
      if(distrConfig()$nVar > 1){
        tryCatch({histogramMaker(
          (paramsTransformed() %>%  as.matrix())[,1],
          paste0("$",distrConfig()$intrParamTex, "$"))}, error = function(e){ggplot2::element_blank()})
      } else{ggplot2::element_blank()}},
      height = if(distrConfig()$nVar > 1){350} else {1},
      width = if(distrConfig()$nVar > 1){350} else {1})

    output$ordinalPlot <- if(distrConfig()$distrGroup == "Ordered" ){
      renderPlot({
        req(paramsTransformed())
        tryCatch(
          {orderedDistSpecialPlot(parser(distrConfig()$yStarPDF),paramsTransformed())},
          error = function(e){ggplot2::element_blank()}) },
        height = 350, width = 350)
    } else {renderPlot({ggplot2::element_blank()}, height = 1, width = 1)}

    output$ordinalHelper <- renderUI({
      if(
        distrConfig()$distrGroup == "Ordered" ){helperMaker("Ordinal Plot")}
      else {div()}})
  })

  observeEvent({probParams()},{
    if(distrConfig()$nCovar > 1) {req(xVals())}
    testVals <- round(rnorm(1, 2),5)
    if((parser(distrConfig()$transformFun))(testVals, xVals(), DGP = T) != testVals){
      margNumFFP <- substr(input$marginalSelectedP,2,2) %>%
        as.numeric()
      # TODO: why is this plot call such a nightmare
      output$functionalFormPlot <- renderPlot({
        if(distrConfig()$distrGroup == "Ordered"){
          ffFun <- functionalFormPlotOrdered}
        else {ffFun <- functionalFormPlot}

        tryCatch({ ffFun(
          transformFun = parser(distrConfig()$transformFun),
          paramRange = parser(distrConfig()$chartDomain)(distrConfig()$nCovar)[[1]],
          paramTex = parser(distrConfig()$paramList)[[margNumFFP]],
          intrParamTex = distrConfig()$intrParamTex,
          fixValues = probParams(),
          multi = (distrConfig()$nVar != 1),
          margNum = margNumFFP,
          xVals = xVals(),
          xChoice = xChoices(),
          funcRange = parser(distrConfig()$funcFormRange),
          pdfFun = parser(distrConfig()$pdfList))}
          , error = function(e){ggplot2::element_blank()})
      } ,
      height = 350, width = 350)
      #TODO: how can this call be shorter tho

    } else {
      output$functionalFormPlot  <- renderPlot({ggplot2::element_blank()}, height = 1, width = 1)
    }

  })

  ########### probability tab data gen #############
  output$dataHeader <- renderUI({dataHeaderFun(distrConfig()$distrGroup)})
  outcomeData <- reactive({
    req(paramsTransformed())
    tryCatch({
      parser(distrConfig()$drawFun)(param = paramsTransformed(), nObs = input$nObs)},
      error = function(e){NULL}) })
  output$outcomeDisplayP <- renderText({
    req(outcomeData())
    dataPrintHelper(outcomeData(), 200)})

  ####################################################################################
  # MLE Tab
  ####################################################################################

  ########### MLE tab title #############
  titleTextAssumed <- reactiveVal()
  observeEvent(
    input$distrID,
    {
      #TODO: fix to be less janky

      shinyjs::disable(selector=".navbar-nav > li:nth-child(3)")
      shinyjs::disable(selector=".navbar-nav > li:nth-child(3) a")
      titleTextAssumed(
        div(
          icon("chevron-right"),
          tags$b("Model: ---"),
          style = "color:#c59267;",
          title = "Likelihood Inference Tab",
          helperMakerNavbar(str = "Likelihood Inference (Disabled)")
        )

      )
    })

  observeEvent(
    outcomeData(),
    {
      shinyjs::enable(selector=".navbar-nav > li:nth-child(3)")
      shinyjs::enable(selector=".navbar-nav > li:nth-child(3) a")

      titleTextAssumed(
        div(icon("chevron-right"),
            tags$b("Model: ---"),
            style = "color:#fff;",
            title = "Likelihood Inference Tab",
            helperMakerNavbar(str = "Likelihood Inference")
        ))
    })


  observeEvent(input$assumedDistrID,
               titleTextAssumed(
                 div(
                   icon("chevron-right"),
                   tags$b("Model: "),
                   input$assumedDistrID,
                   title = "Likelihood Inference Tab",
                   helperMakerNavbar(str = "Likelihood Inference"))))
  output$assumedDistrNameOutput <- renderUI({
    titleTextAssumed()

  })


  output$assumedDistrNameOutput <- renderUI({
    titleTextAssumed()
  })

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
    distrDF %>%  dplyr::filter(distrList == input$assumedDistrID)
  })


  ########### LHS UI #############

  numXAssumed <- reactiveVal(NULL)
  observeEvent(input$assumedDistrID, {
    req(assumedDistrConfig())
    if((assumedDistrConfig()$nCovar > 1)){ numXAssumed(2)} else {numXAssumed(1)}
    byHandParams <- byHandTransformed <- assumedXChoices <- assumedXVals <-  reactive({NULL})

    output$assumedXChoiceDiv  <- renderUI({
      if(assumedDistrConfig()$nCovar > 1 ){
        xChoiceDivFun(
          choices = if(input$assumedDistrID == input$distrID) {xChoices()[1:(numXAssumed()-1)]}
          else if(!is.null(assumedXChoices())){
            if(sum(!is.na(assumedXChoices())) == (numXAssumed()-1)){
              assumedXChoices() } else {
                c(assumedXChoices()[!is.na(xChoices())], defaultXChoices[numXAssumed()-1])  }
          } else {defaultXChoices[1:(numXAssumed()-1)]},
          plus = (assumedDistrConfig()$nCovar > numXAssumed()),
          minus = (numXAssumed() > 2),
          assumed = T)
      } else{xChoiceDivFun(hidden=T)}})

  })

  observeEvent(input$addXVarAssumed, {
    req(numXAssumed())
    if(numXAssumed() < assumedDistrConfig()$nCovar) {
      numXAssumed(numXAssumed() + 1)
      byHandParams <- byHandTransformed <- assumedXChoices <- assumedXVals <-  reactive({NULL})
    }
  })

  observeEvent(input$subtractXVarAssumed, {
    req(numXAssumed())
    if(numXAssumed() > 2) {
      numXAssumed(numXAssumed() - 1)
      byHandParams <- byHandTransformed <- assumedXChoices <- assumedXVals <-  reactive({NULL})
    }
  })

  output$statModel <- renderUI({
    req(input$assumedDistrID)
    if(assumedDistrConfig()$nCovar > 1) {req(numXAssumed())}
    parser(assumedDistrConfig()$latexList)(nXValsAssumed = numXAssumed()-1,
                                           type = "Model")})

  output$likelihood <- renderUI({
    req(input$assumedDistrID)
    parser(assumedDistrConfig()$latexList)(type = "Likelihood")
  })


  ########### RHS UI #############
  output$paramByHandSlider <-renderUI({
    req(assumedDistrConfig())
    if(assumedDistrConfig()$nCovar > 1) {req(numXAssumed())}
    manyParamSliderMaker(
      minVal = assumedDistrConfig()$sliderMinA,
      maxVal = assumedDistrConfig()$sliderMaxA,
      startVals = parser(assumedDistrConfig()$sliderStarts)[1:(numXAssumed() + assumedDistrConfig()$nNonXParams)],
      sigmaScale =  parser(assumedDistrConfig()$gammaScale),
      paramTex = assumedDistrConfig()$paramTex,
      inputName = "byHand")
  })

  mcListLL <- reactive({
    if (assumedDistrConfig()$nVar > 1){
      firstParamName <- capitalizeStr(substr(assumedDistrConfig()$paramTex, 2, nchar(assumedDistrConfig()$paramTex)))
      if(assumedDistrConfig()$secondParamTex != "NA"){
        secondParamName <- "Gamma"
        c(lapply(0:(numXAssumed()-1), function(i){paste0(firstParamName,i)} ), secondParamName )
      } else {
        lapply(0:(numXAssumed()-1), function(i){paste0(firstParamName,i)} )

      }
    }
  })

  output$marginalSelectorLL <- renderUI({
    req(numXAssumed())
    if (assumedDistrConfig()$nVar > 1){
      marginalSelectInput(choicesInput = mcListLL(),
                          inputID = "marginalSelectedLL")
    } else{marginalSelectInput(hidden = T)}})

  output$marginalSelectorLLF   <- renderUI({
    req(numXAssumed())
    if (assumedDistrConfig()$nVar > 1){
      marginalSelectInput(choicesInput = paste0("X",(1:(numXAssumed()-1))),
                          inputID = "marginalSelectedLLF")
    } else{marginalSelectInput(hidden = T)}})


  ########### Computations #############
  #TODO: this feels redundant... modules?
  byHandParams <- reactive({
    req(numXAssumed())
    vec <- input$byHand1
    if(!isnothing(input$byHand2)){vec <- c(vec, input$byHand2)}
    if(!isnothing(input$byHand3)){vec <- c(vec, input$byHand3)}
    if(!isnothing(input$byHand4)){vec <- c(vec, input$byHand4)}
    if(!isnothing(input$byHand5)){vec <- c(vec, input$byHand5)}
    vec[1:(numXAssumed() + assumedDistrConfig()$nNonXParams)]
  })

  assumedXChoices <- reactive({
    vec <- c()
    if(!isnothing(input$assumedXChoice1)){vec <- c(vec, input$assumedXChoice1)}
    if(!isnothing(input$assumedXChoice2)){vec <- c(vec, input$assumedXChoice2)}
    if(!isnothing(input$assumedXChoice3)){vec <- c(vec, input$assumedXChoice3)}
    if(!isnothing(input$assumedXChoice4)){vec <- c(vec, input$assumedXChoice4)}
    vec[1:(numXAssumed()-1)]
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
                  function(i){(parser(assumedDistrConfig()$transformFun))(byHandParams(), assumedXVals()[i,], DGP = F)})
    if(!is.null(dim(vec))){vec %>%  t()} else {vec} ##TODO: figure out how to use apply to avoid
  })

  margNumLL <- eventReactive({
    input$assumedDistrID
    input$marginalSelectedLL
  },{
    tmp <- which(mcListLL() == input$marginalSelectedLL)
    if(length(tmp) == 0){1} else{tmp}
  })

  MLEResult <- reactive({

    req(outcomeData())
    if(assumedDistrConfig()$nCovar > 1) {req(margNumLL(), assumedXVals())}
    likelihoodEstimateFun(
      chartDomain = parser(assumedDistrConfig()$chartDomain)(numXAssumed() + assumedDistrConfig()$nNonXParams),
      likelihoodFun = parser(assumedDistrConfig()$likelihoodFun),
      margNum = margNumLL(),
      outcome = outcomeData(),
      xVals = assumedXVals(),
      optimMethod = assumedDistrConfig()$optimMethod,
      nParams = numXAssumed() + assumedDistrConfig()$nNonXParams)
  })

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
      distrDF,
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


  observeEvent({
    input$assumedDistrID
    MLEResult()
  },{
    testVals <- round(rnorm(1, 2),5)
    if((parser(assumedDistrConfig()$transformFun))(testVals, assumedXVals()) != testVals){
      margNumLLF <- substr(input$marginalSelectedLLF,2,2) %>%  as.numeric()
      if(length(margNumLLF) == 0){margNumLLF <- 1}

      output$functionalFormPlotLL <- renderPlot({

        if(assumedDistrConfig()$distrGroup == "Ordered"){
          ffFunLL <- functionalFormPlotOrdered}
        else {ffFunLL <- functionalFormPlot}

        ffFunLL(
          transformFun = parser(assumedDistrConfig()$transformFun),
          paramRange = parser(assumedDistrConfig()$chartDomain)(assumedDistrConfig()$nCovar)[[1]],
          paramTex = parser(assumedDistrConfig()$paramList)[[margNumLLF]],
          intrParamTex = assumedDistrConfig()$intrParamTex,
          fixValues = byHandParams(),
          multi = (assumedDistrConfig()$nVar != 1),
          margNum = margNumLLF,
          xVals = assumedXVals(),
          xChoice = assumedXChoices(),
          funcRange = parser(assumedDistrConfig()$funcFormRange),
          pdfFun = parser(assumedDistrConfig()$pdfList))},
        height = 350, width = 350)

      #TODO: how can this call be shorter tho

    } else {
      output$functionalFormPlot  <- renderPlot({ggplot2::element_blank()}, height = 1, width = 1)
    }
  })

  output$MLEParamLatex <- renderUI({
    req(MLEResult())
    coeffLatex(assumedDistrConfig()$paramTex,assumedDistrConfig()$reparamTex, MLEResult()$paramHat )})
  output$MLEVcovLatex <- renderUI({
    req(MLEResult())
    vCovLatex(assumedDistrConfig()$paramTex, MLEResult()$paramVCov )})



}


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
  
  numX <- reactiveVal(NULL)
  observeEvent(input$distrID, {# Reset/invalidate some stuff
    output$functionalFormPlot  <- renderPlot({element_blank()}, height = 1, width = 1)
    probParams <- paramsTransformed <- xChoices <- xVals <-  reactive({NULL})
    
    output$xChoiceDiv  <- renderUI({
      if(distrConfig()$nCovar > 1 ){
        xChoiceDivFun(
          choices = if(!is.null(xChoices())){
            if(sum(!is.na(xChoices())) == (numX()-1)){
              xChoices() } else { c(xChoices()[!is.na(xChoices)], defaultXChoices[numX()-1])  }
          } else {defaultXChoices[1:(numX()-1)]},
          plus = (distrConfig()$nCovar > numX()))
      } else{xChoiceDivFun(hidden=T)}})
    
    MLEResult <- reactive({NULL})
    if((distrConfig()$nCovar > 1)){ numX(2)} else {numX(1)}
    
  })
  
  # set up all the main user inputs
  output$distrNameOutput <- renderUI({div(tags$b("DGP: "),input$distrID)})
  output$obsSlider <- renderUI({if(distrConfig()$distrGroup != "Real"){obsSliderFun(distrConfig()$nVar)} else div() })



output$paramSlider <-renderUI({
  manyParamSliderMaker(
    minVal = distrConfig()$sliderMin,
    maxVal = distrConfig()$sliderMax,
    startVals = parser(distrConfig()$sliderStarts)[1:(numX() + distrConfig()$nNonXParams)],
    sigmaScale =  parser(distrConfig()$sigmaScale), 
    paramTex = distrConfig()$paramTex)
})

observeEvent(input$addXVar, {
  req(numX())
  if(numX() < distrConfig()$nCovar) {
    numX(numX() + 1)
    probParams <- paramsTransformed <- xChoices <- xVals <-  reactive({NULL})
  }
})
# get the input parameters
output$distrTex <- renderUI({
  latexSwitcher(input$distrID, type = "Distr", nXValsPDF = numX()-1) })

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
      tryCatch({histogramMaker((paramsTransformed() %>%  as.matrix())[,1],
                               paste0("$",distrConfig()$intrParamTex, "$"))}, error = function(e){element_blank()})
    } else{element_blank()}},
    height = if(distrConfig()$nVar > 1){350} else {1},
    width = if(distrConfig()$nVar > 1){350} else {1})
  
  output$specialPlot <- if(distrConfig()$distrGroup == "Ordered" ){
    renderPlot({
      req(paramsTransformed())
      tryCatch(
        {orderedDistSpecialPlot(parser(distrConfig()$yStarPDF),paramsTransformed())},
        error = function(e){element_blank()}) },
      height = 350, width = 350)
  } else {renderPlot({element_blank()}, height = 1, width = 1)}
  
})

observeEvent({probParams()},{
  if(distrConfig()$nCovar > 1) {req(xVals())}
  testVals <- round(rnorm(1, 2),5)
  if((parser(distrConfig()$transformFun))(testVals, xVals()) != testVals){
    margNumFFP <- substr(input$marginalSelectedP,2,2) %>%  as.numeric()
    tryCatch({
    output$functionalFormPlot <- renderPlot({functionalFormPlotSwitcher(
      input$distrID,
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
      pdfFun = parser(distrConfig()$pdfList))},
      height = 350, width = 350)
    }, error = function(e){element_blank()})
    
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
outcomeData <- reactive({
  req(paramsTransformed())
  tryCatch({
    parser(distrConfig()$drawFun)(param = paramsTransformed(), nObs = input$nObs)},
    error = function(e){NULL}) })
output$outcomeDisplayP <- renderText({
  req(outcomeData())
  dataPrintHelper(outcomeData(), 200)})

############################
# MLE Tab
############################


########### tab title #############
titleTextAssumed <- reactiveVal()
observeEvent(input$distrID, titleTextAssumed(div(icon("chevron-right"),  tags$b("Model: ---"))))
observeEvent(input$assumedDistrID,titleTextAssumed(div(icon("chevron-right"), tags$b("Model: "),input$assumedDistrID)))
output$assumedDistrNameOutput <- renderUI({titleTextAssumed()})


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
              c(assumedXChoices()[!is.na(xChoices)], defaultXChoices[numXAssumed()-1])  }
        } else {defaultXChoices[1:(numXAssumed()-1)]},
        plus = (assumedDistrConfig()$nCovar > numXAssumed()), assumed = T)
    } else{xChoiceDivFun(hidden=T)}})
  
  # output$assumedXChoiceDiv <- renderUI({
  #   if (assumedDistrConfig()$nVar > 1){xChoiceDivFun(
  #     choices = if(input$assumedDistrID == input$distrID) {xChoices()
  #     } else {defaultXChoices[1:(assumedDistrConfig()$nCovar-1)]}, assumed=T)}
  #   else {xChoiceDivFun(hidden = T)}})
})

observeEvent(input$addXVarAssumed, {
  req(numXAssumed())
  if(numXAssumed() < assumedDistrConfig()$nCovar) {
    numXAssumed(numXAssumed() + 1)
    byHandParams <- byHandTransformed <- assumedXChoices <- assumedXVals <-  reactive({NULL})
  }
})

output$statModel <- renderUI({
  req(input$assumedDistrID)
  if(assumedDistrConfig()$nCovar > 1) {req(numXAssumed())}
  latexSwitcher(input$assumedDistrID, nXValsAssumed = numXAssumed()-1,
                type = "Model")})

output$likelihood <- renderUI({
  req(input$assumedDistrID)
  latexSwitcher(input$assumedDistrID, type = "Likelihood")})

########### RHS UI #############
output$paramByHandSlider <-renderUI({
  req(assumedDistrConfig())
  if(assumedDistrConfig()$nCovar > 1) {req(numXAssumed())}
  manyParamSliderMaker(
    minVal = assumedDistrConfig()$sliderMin,
    maxVal = assumedDistrConfig()$sliderMax,
    startVals = parser(assumedDistrConfig()$sliderStarts)[1:(numXAssumed() + assumedDistrConfig()$nNonXParams)],
    sigmaScale =  parser(assumedDistrConfig()$sigmaScale), 
    paramTex = assumedDistrConfig()$paramTex,
    inputName = "byHand")
})

output$marginalSelectorLL <- renderUI({
  if (assumedDistrConfig()$nVar > 1){
    firstParamName <- capitalizeStr(substr(assumedDistrConfig()$paramTex, 2, nchar(assumedDistrConfig()$paramTex)))
    secondParamName <-capitalizeStr(substr(assumedDistrConfig()$secondaryParamTex, 2, nchar(assumedDistrConfig()$secondaryParamTex)))
    
    mcList <- c(lapply(0:(numXAssumed()-1), function(i){paste0(firstParamName,i)} ), secondParamName )
    marginalSelectInput(choicesInput = mcList,
                        inputID = "marginalSelectedLL")
  } else{marginalSelectInput(hidden = T)}})

output$marginalSelectorLLF   <- renderUI({
  if (assumedDistrConfig()$nVar > 1){
    marginalSelectInput(choicesInput = paste0("X",(1:(numXAssumed()-1))),
                        inputID = "marginalSelectedLLF")
  } else{marginalSelectInput(hidden = T)}})


########### Computations #############
#TODO: this feels redundant... modules?
byHandParams <- reactive({
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
                function(i){(parser(assumedDistrConfig()$transformFun))(byHandParams(), assumedXVals()[i,])})  
  if(!is.null(dim(vec))){vec %>%  t()} else {vec} ##TODO: figure out how to use apply to avoid
  
})

margNumLL <- eventReactive({
  input$assumedDistrID
  input$marginalSelectedLL
  },{
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
    optimMethod = assumedDistrConfig()$optimMethod,
    nParams = numXAssumed() + assumedDistrConfig()$nNonXParams)})


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
  coeffLatex(assumedDistrConfig()$paramTex,assumedDistrConfig()$secondaryParamTex, MLEResult()$paramHat )})
output$MLEVcovLatex <- renderUI({
  req(MLEResult())
  vCovLatex(assumedDistrConfig()$paramTex, MLEResult()$paramVCov )})


#########
# Sim
#########


########### tab title #############
titleTextSim <- reactiveVal()
observeEvent(input$distrID, titleTextSim(div(icon("chevron-right"),  tags$b("Quantities of Interest"), style = "color:#999999")))
observeEvent(input$assumedDistrID, titleTextSim(div(icon("chevron-right"),  tags$b("Quantities of Interest"), style = "color:#ffffff")))
output$simTitleOutput <- renderUI({titleTextSim()})


########### UI #############
# TODO: clean up error handling

output$simHeader <- renderUI({
  tryCatch(MLEResult(), error = function(e){tags$b("Please Choose A Model", style = "color:red")})
  if( (is.null(input$assumedDistrID)) ||(is.null(MLEResult()))){ tags$b("Please Choose A Model", style = "color:red")
  } else { tags$p(tags$b("From Likelihood Tab"))} 
})

output$simParamLatex <- renderUI({
  req(MLEResult())
  coeffLatex(assumedDistrConfig()$paramTex,assumedDistrConfig()$secondaryParamTex, MLEResult()$paramHat ) })


output$simVcovLatex <- renderUI({
  req(MLEResult())
  vCovLatex(parser(assumedDistrConfig()$paramList), MLEResult()$paramVCov )})

output$simSliders  <-  renderUI({
  req(MLEResult())
  simMultiSliderFunction(numXAssumed()-1)})
output$simEstimationLatex <-  renderUI({
  req(MLEResult())
  latexSwitcher(
    input$assumedDistrID,
    type = "Estimation Uncertainty",
    paramTex = assumedDistrConfig()$paramTex
  )})


output$pickQOIBox <- renderUI({
  req(MLEResult())
  QOISwitcher(input$assumedDistrID)})

output$marginalSelectorSim <- renderUI({
  req(MLEResult())
  if (assumedDistrConfig()$nVar > 1){
    marginalSelectInput(choicesInput = paste0("X",1:(numXAssumed()-1)),
                        inputID = "marginalSelectedSim")
  } else{marginalSelectInput(hidden = T)}})


########### computation #############

simXVals <- reactive({
  vec <- c(1)
  if(!is.null(input$simX1)){vec <- c(vec, input$simX1)}
  if(!is.null(input$simX2)){vec <- c(vec, input$simX2)}
  if(!is.null(input$simX3)){vec <- c(vec, input$simX3)}
  if(!is.null(input$simX4)){vec <- c(vec, input$simX4)}
  vec[1:(numXAssumed())]
})

output$simFundamentalLatex <-  renderUI({
  req(MLEResult())
  if(assumedDistrConfig()$nCovar > 1) {req(simXVals())}
  latexSwitcher(
    input$assumedDistrID,
    type = "Fundamental Uncertainty",
    xValsSim = simXVals(),
    paramTex = assumedDistrConfig()$paramTex,
    intrParamTex = assumedDistrConfig()$intrParamTex,
  )})


# Simulate in stages. First get the base parameters (betas). Then get
# the intermediate parameters (pi, mu, lambda etc)
# Then draw ys as appropriate. 
# For non-covariate distrs, some of these steps are trivial
paramTilde <- reactive({
  req(MLEResult())
  paramTildeCreator(
    paramHat = MLEResult()$paramHat,paramVCov =  MLEResult()$paramVCov, 1000)})
intrTilde <- reactive({
  req(paramTilde())
  if(assumedDistrConfig()$nCovar > 1) {req(simXVals())}
  intrTildeCreator(paramTilde(),parser(assumedDistrConfig()$transformFun), simXVals()) })
yTilde<- reactive({
  req(intrTilde())
  yTildeCreator(intrTilde(),model = parser(assumedDistrConfig()$drawFun))})



observeEvent({paramTilde()},{
  testVals <- round(rnorm(1, 2),5)
  if(((parser(assumedDistrConfig()$transformFun))(testVals, assumedXVals()) != testVals) &
     (assumedDistrConfig()$distrGroup != "Ordered" ) &  (assumedDistrConfig()$nVar != 1)){
    output$functionalFormPlotSim <- renderPlot({
      functionalFormWithCI(transformFun = parser(assumedDistrConfig()$transformFun),
                           fixValuesX = simXVals(),
                           paramTildes = paramTilde(),
                           funcRange = parser(assumedDistrConfig()$funcFormRange),
                           margNum = substr(input$marginalSelectedSim,2,2) %>%  as.numeric(),
                           intrParamTex = assumedDistrConfig()$intrParamTex )}, height = 350)
    
    output$SimHover_info <- renderUI({
      if(assumedDistrConfig()$nCovar > 1){
        tooltipFun(input$SimPlot_hover, "Other X fixed at chosen values")} else {div()}  })
    
  } else {output$functionalFormPlotSim <-  renderPlot({element_blank()}, height = 1, width = 1)}
  
})

QOIOutputs <- reactive({
  req(yTilde())
  if(assumedDistrConfig()$nCovar > 1) {req(simXVals())}
  QOIVisualization(yTilde(), intrTilde(), assumedDistrConfig(), input$QOIid)})

output$QOIChart  <- renderPlot({
  req(QOIOutputs())})


}

# Run the application 
shinyApp(ui = ui, server = server,
         onStart = function(){
           oldw <<- getOption("warn")
           options(warn = -1)#, shiny.fullstacktrace = T)
           onStop(function(){
             options(warn = oldw)
             
           })
           
         })

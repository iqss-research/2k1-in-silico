#' model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_model_tab_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = uiOutput(ns("assumedDistrNameOutput")),
    value ="Likelihood",
    fluidRow(
      column(
        12,
        tags$p(tags$b("Generated Y (from DGP Tab)")),
        div(htmlOutput(ns("outcomeDisplayL")),
            helperMaker("Data for Inference", styleArg = "left:350px"),
            style= "padding-left:15px;")
      ),
      style = "padding-bottom:10px;"
    ),
    hr(),
    fluidRow(
      column(
        3,id = "assumedDistrSelectCol",
        uiOutput(ns("assumedDistrSelect")),
        helperMaker("Model Selection")
      ) # depends on actual
    ),
    fluidRow(
      column(
        5,
        column(
          12,
          uiOutput(ns("assumedXChoiceDiv"),
                   style = "padding-left:15px;"),
          helperMaker("Hypothesize a Covariate"),
        ),
        column(
          12,
          id = "statModelRow",
          uiOutput(ns("statModel")),
          helperMaker("Statistical Model")
        ),
        hr(),
        column(
          12,
          id = "likelihoodRow",
          uiOutput(ns("likelihood")),
          helperMaker("Likelihood")
        ),
        hr(),
        tags$p(tags$b("Maximum Likelihood Estimates")),
        column(
          12, id = "estimatesRow",
          uiOutput(ns("MLEParamLatex"),
                   style = "float:left;padding-left:30px;padding-top:10px;"),
          uiOutput(ns("MLEVcovLatex"),
                   style = "float:left;padding-left:30px;padding-top:10px;"),
          helperMaker("Estimates")
        ),
        style = "padding-left:30px",
      ),
      column(
        6, id = "guesstimateCol",
        style = "width:400px",
        tags$p(tags$b("Guesstimate"),
               style = paste0("color:", baseColor2)),
        div(
          uiOutput(ns("paramByHandSlider")),
          style= "padding-left:15px;float:left;"),
        div(actionButton(ns("resetByHand"),
                         label = "Set to MLE",
                         title = "Set Guesstimates to MLE"),
            style = "padding-left:30px;padding-bottom:10px;float:left;"),
        column(12,
               plotOutput(ns("MLEByHandPlot"),
                          height = "auto"),
               title = "Guesstimate vs. Observed Data",
               helperMaker("Guesstimate Plot")
        ),
        helperMaker("Guesstimate")
      )
    ),
    fluidRow(
      column(
        6, offset = 5,
        column(
          12,
          plotOutput(ns("MLEPlot"), height = "300px"),
          title = "Other Parameters fixed at MLEs",
          helperMaker("Likelihood Plot")
        ),
        column(8,
               offset = 4,
               uiOutput(ns("marginalSelectorLL"))),
        br(),
        column(
          12,
          uiOutput(ns("ffPlotLLUI")),
          title = "Other X fixed at means, parameters fixed at MLEs",
          helperMaker("Functional Form (Model)")
        ),

        column(8,
               offset = 4,
               uiOutput(ns("marginalSelectorLLF"))),
      ),
      style = "padding-left:15px;"

    )
  )


}

#' model Server Functions
#'
#' @noRd
mod_model_tab_server <- function(id, distrConfig, outcomeData,
                                 xChoices){
  # confirm reactive values from dgp tab passed through
  stopifnot(is.reactive(distrConfig))
  stopifnot(is.reactive(outcomeData))
  stopifnot(is.reactive(xChoices))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    titleTextAssumed <- reactiveVal(
      div(
        icon("chevron-right"),
        tags$b("Model: ---"),
        style = "color:#c59267;",
        helperMakerNavbar(str = "Likelihood Inference (Disabled)")
      )
    )

    observeEvent(
      distrConfig()$distrList,
      {
        #TODO: fix to be less janky

        shinyjs::disable(selector=".navbar-nav > li:nth-child(3)")
        shinyjs::disable(selector=".navbar-nav > li:nth-child(3) a")
        titleTextAssumed(
          div(
            icon("chevron-right"),
            tags$b("Model: ---"),
            style = "color:#c59267;",
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
              helperMakerNavbar(str = "Likelihood Inference")
          ))
      })


    observeEvent(input$assumedDistrID,
                 titleTextAssumed(
                   div(
                     icon("chevron-right"),
                     tags$b("Model: "),
                     input$assumedDistrID,
                     helperMakerNavbar(str = "Likelihood Inference"))))

    output$assumedDistrNameOutput <- renderUI({
      titleTextAssumed()

    })

    ########### top UI #############
    output$outcomeDisplayL  <- renderText({dataPrintHelper(outcomeData(), 200)})


    output$assumedDistrSelect  <- renderUI({
      div(selectInput(
        inputId = ns("assumedDistrID"),
        label = tags$p(tags$b("Assumed Distribution"),style = "font-size:15px; !important"),
        choices = parser_lst(distrConfig()$assumedDistrChoices),
        width = "250px"), class = "distrInput")
    })

    assumedDistrConfig <- reactive({
      req(input$assumedDistrID)
      distrDF[.(input$assumedDistrID)]
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
            ns = ns,
            choices = if(input$assumedDistrID == distrConfig()$distrList) {xChoices()[1:(numXAssumed()-1)]}
            else if(!is.null(assumedXChoices())){
              if(sum(!is.na(assumedXChoices())) == (numXAssumed()-1)){
                assumedXChoices() } else {
                  c(assumedXChoices()[!is.na(xChoices())],
                    defaultXChoices[numXAssumed()-1])  }
            } else {defaultXChoices[1:(numXAssumed()-1)]},
            plus = (assumedDistrConfig()$nCovar > numXAssumed()),
            minus = (numXAssumed() > 2), assumed = T)
        } else{xChoiceDivFun(ns=ns, hidden=T)}})

    })

    observeEvent(input$addXVarAssumed, {
      req(numXAssumed(), assumedDistrConfig())
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
      parser(assumedDistrConfig()$latexList)(
        nXValsAssumed = numXAssumed()-1, type = "Model")})

    output$likelihood <- renderUI({
      req(input$assumedDistrID)
      parser(assumedDistrConfig()$latexList)(type = "Likelihood")
    })

    mcListLL <- reactive({
      if (assumedDistrConfig()$nVar > 1){
        firstParamName <- capitalizeStr(
          substr(assumedDistrConfig()$paramTex, 2,
                 nchar(assumedDistrConfig()$paramTex)))
        if(!is.na(assumedDistrConfig()$secondParamTex)){
          secondParamName <- "Gamma"
          c(lapply(0:(numXAssumed()-1), function(i){paste0(firstParamName,i)} ),
            secondParamName )
        } else {
          lapply(0:(numXAssumed()-1), function(i){paste0(firstParamName,i)} )

        }
      }
    })

    output$marginalSelectorLL <- renderUI({
      req(numXAssumed())
      if (assumedDistrConfig()$nVar > 1){
        marginalSelectInput(ns=ns,
                            choicesInput = mcListLL(),
                            inputID = "marginalSelectedLL")
      } else{marginalSelectInput(ns=ns,hidden = T)}})

    output$marginalSelectorLLF   <- renderUI({
      req(numXAssumed())
      if (assumedDistrConfig()$nVar > 1){
        marginalSelectInput(ns=ns,
                            choicesInput = paste0("X",(1:(numXAssumed()-1))),
                            inputID = "marginalSelectedLLF")
      } else{marginalSelectInput(ns=ns, hidden = T)}})

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

    output$MLEParamLatex <- renderUI({
      req(MLEResult())
      coeffLatex(assumedDistrConfig()$paramTex,
                 assumedDistrConfig()$reparamTex,
                 MLEResult()$paramHat )})

    output$MLEVcovLatex <- renderUI({
      req(MLEResult())
      vCovLatex(assumedDistrConfig()$paramTex,
                MLEResult()$paramVCov )})

    # reset to MLE button
    observeEvent(input$resetByHand, {
      req(MLEResult())
      lapply(1:assumedDistrConfig()$nVar, function(i){
        updateSliderInput(inputId = paste0("byHand",i),
                          value = MLEResult()$paramHat[i],
                          session = session)}) })


    # ########### RHS UI #############
    output$paramByHandSlider <-renderUI({
      req(assumedDistrConfig())
      if(assumedDistrConfig()$nCovar > 1) {req(numXAssumed())}
      manyParamSliderMaker(
        ns=ns,
        minVal = assumedDistrConfig()$sliderMinA,
        maxVal = assumedDistrConfig()$sliderMaxA,
        startVals = parser(assumedDistrConfig()$sliderStarts)[1:(numXAssumed() + assumedDistrConfig()$nNonXParams)],
        sigmaScale =  parser(assumedDistrConfig()$gammaScale),
        paramTex = assumedDistrConfig()$paramTex,
        inputName = "byHand")
    })


    ########### Computations #############
    byHandParams <- reactive({
      req(numXAssumed())
      inputsClean(input, "byHand", numXAssumed() + assumedDistrConfig()$nNonXParams)
    })

    assumedXChoices <- reactive({
      req(numXAssumed())
      inputsClean(input, "assumedXChoice", numXAssumed() -1)
    })

    assumedXVals <- reactive({
      req(assumedXChoices())
      tryCatch({
        if(assumedDistrConfig()$nCovar > 1){
          xValGenerator(length(outcomeData()),assumedXChoices())} else {NULL}
      }, error = function(e){NULL})
    })

    byHandTransformed <- reactive({
      req(byHandParams())
      if(assumedDistrConfig()$nCovar > 1) {req(assumedXVals())}
      vec <- sapply(
        1:length(outcomeData()),
        function(i){
          (parser(assumedDistrConfig()$transformFun))(
            byHandParams(),
            assumedXVals()[i,], DGP = F)})
      if(!is.null(dim(vec))){vec %>%  t()} else {vec} ##TODO: figure out how to use apply to avoid
    })

    ########### Plots #############

    testMLEbyHand <-  reactive({
      handMLESwitcher(
        input$assumedDistrID,
        distrDF,
        data = outcomeData(),
        domain = parser(assumedDistrConfig()$analyticDomain),
        range = parser(assumedDistrConfig()$analyticRange),
        pdfFun = parser(assumedDistrConfig()$pdfList),
        assumedParam = byHandTransformed() %>%  as.matrix(),
        multiModel = (assumedDistrConfig()$nVar != 1))
    })

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
      MLEPlotFun(MLEResult(), parser_vec(assumedDistrConfig()$paramList)[[margNumLL()]])
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
      input$marginalSelectedLLF
      MLEResult()
    },{
      testVals <- round(stats::rnorm(1, 2),5)
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
            paramTex = parser_vec(assumedDistrConfig()$paramList)[[margNumLLF]],
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

        output$ffPlotLLUI <- renderUI({plotOutput(outputId = "functionalFormPlotLL", inline = T)})

      } else {
        output$ffPlotLLUI  <- renderUI({div()})
      }
    }, ignoreNULL = F)


    return(list(assumedDistrConfig=assumedDistrConfig,
                MLEResult=MLEResult,
                numXAssumed=numXAssumed,
                assumedXVals=assumedXVals))
  })
}

## To be copied in the UI
# mod_model_tab_ui("model_tab_1")

## To be copied in the server
# mod_model_tab_server("model_tab_1")
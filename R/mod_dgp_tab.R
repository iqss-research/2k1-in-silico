#' test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dgp_tab_ui <- function(id){
  ns <- NS(id)
  #tagList(
  shinybrowser::detect()
    tabPanel(

      title = uiOutput(ns('distrNameOutput')),
      value = "dgp",

      fluidRow(
        column(
          12,
          div(
            selectInput(
              inputId = ns("distrID"),
              label = tags$p(
                tags$b("Data Generation Process"),
                style = "font-size:15px; !important"),
              choices = optGroups,
              selected = selectedDist,
              width = "250px"),
            class = "distrInput"),
        hr(),
        column(4, id = "sliders",
               fluidRow(
                 column(
                   12,
                   uiOutput(ns("distrTex")),
                   helperMaker("Probability Model")
                 ),
                 column(
                   12,
                   uiOutput(ns("obsSlider")),
                   helperMaker("Observation Choice")
                 ),
                 column(
                  12,
                  uiOutput( #TODO: toggle divs with removeUI
                    ns("xChoiceDiv"),
                    style = "padding-left:15px;"),
                  helperMaker("Covariates")
                 ),
                 #textOutput(ns("browserwidth"),inline=TRUE),
                 uiOutput(ns("paramSlider"))
                ),
               hr(),
               column(
                 12,
                 fluidRow(
                   uiOutput(ns("dataHeader")),
                   div(
                     textOutput(ns("outcomeDisplayP")),
                     style= "padding-top:15px;padding-left:15px",
                     width = "50px"),
                   helperMaker("Randomly Generated Data"),
                 )
               ),

        ),
        column(
          6,
            column(
              12,
              #shinycssloaders::withSpinner(
                plotOutput(ns("distPlot"),
                           inline = T)
              #)
              ,
              title = "Conditional Distribution of Y",
              helperMaker("Analytical Plot", styleArg = "left:375px;")
            ),
          br(),
          column(
              12,
              uiOutput(ns("ordinalPlotUI"), inline = T),
              title = "(Unobserved) Underlying Variable",
              uiOutput(ns("ordinalHelper"))
            ),
          br(),
          column(
              12,
              uiOutput(ns("probHistUI"), inline = T),
              title = "Distribution of intermediate parameter",
              uiOutput(ns("probHistHelper")),
            ),
          column(
              12,
              uiOutput(ns("functionalFormPlotUI"), inline = T),
              title = "Other X fixed at means, parameters at chosen values",
              uiOutput(ns("functionalFormHelper"))
            ),
          uiOutput(ns("marginalSelectorP"), style = "padding-left:155px"),


  )
  )
    ))
}

#' test Server Functions
#'
#' @noRd
mod_dgp_tab_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    session$allowReconnect("force")

    distrConfig <- reactive({
      req(input$distrID)
      distrDF[.(input$distrID)]
    })


    output$distrNameOutput <- renderUI({
      div(id = "DGPTitle",
            tags$b("DGP: "), input$distrID,
          helperMakerNavbar(str = "DGPs and Probability"),
          title = "DGPs/Probability Tab"
      )
    })

    output$distrTex <- renderUI({
      #cat(shinybrowser::get_width())
     parser(distrConfig()$latexList)(type = "Distr",
                                       modelDF = distrConfig(),
                                       browserWidth=shinybrowser::get_width(),
                                       nXValsPDF = numX()-1) })

    # user input logic
    output$obsSlider <- renderUI({
      req(distrConfig())
      obsSliderFun(ns, distrConfig()$nVar) })

    numX <- reactiveVal(NULL)

    output$browserwidth <- renderText({
      paste0("width:",shinybrowser::get_width())
    })

    observeEvent(input$distrID, {

      show_getStarted <- FALSE

      # Reset/invalidate some stuff
      output$functionalFormPlot  <- renderPlot({ggplot2::element_blank()}
                                               #, height = 1, width = 1
                                               )
      probParams <- paramsTransformed <- xChoices <- xVals <-  reactive({NULL})

      output$xChoiceDiv  <- renderUI({
        if(distrConfig()$nCovar > 1 ){
          xChoiceDivFun(
            ns = ns,
            choices = if(!is.null(xChoices())){
              if(sum(!is.na(xChoices())) == (numX()-1)){
                xChoices() } else {
                  c(xChoices()[!is.na(xChoices())], defaultXChoices[numX()-1])
                }
            } else {defaultXChoices[1:(numX()-1)]},
            plus = (distrConfig()$nCovar > numX()),
            minus = (numX() > 2)
          )
        } else{xChoiceDivFun(ns=ns, hidden=T)}})

      MLEResult <- reactive({NULL})
      if((distrConfig()$nCovar > 1)){ numX(2) } else {numX(1)}

    })

    output$paramSlider <-renderUI({
      m <- manyParamSliderMaker(
        ns = ns,
        minVal = distrConfig()$sliderMin,
        maxVal = distrConfig()$sliderMax,
        startVals = parser(distrConfig()$sliderStarts)[1:(numX() + distrConfig()$nNonXParams)],
        sigmaScale =  parser(distrConfig()$sigmaScale),
        paramTex = distrConfig()$paramTex,
        secondParamTex  = distrConfig()$secondParamTex)
      m
    })

    # button to add X
    observeEvent(input$addXVar, {
      req(numX())
      if(numX() < distrConfig()$nCovar) {
        numX(numX() + 1)
        probParams <- paramsTransformed <- xChoices <- xVals <-  reactive({NULL})
      }
    })

    # button to remove X
    observeEvent(input$subtractXVar, {
      req(numX())
      if(numX() > 2) {
        numX(numX() - 1)
        probParams <- paramsTransformed <- xChoices <- xVals <-  reactive({NULL})
      }
    })

    ########### probability page computations #############
    probParams <- reactive({
      inputsClean(input, inputName = "param", numX() + distrConfig()$nNonXParams)
    })

    xChoices <- reactive({
      inputsClean(input, inputName = "xChoice", numX()-1)
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

    #### GENERATE DATA ####

    output$dataHeader <- renderUI({dataHeaderFun(distrConfig()$distrGroup)})

    outcomeData <- reactive({
      req(paramsTransformed())
      tryCatch({
        parser(distrConfig()$drawFun)(param = paramsTransformed(),
                                      nObs = input$nObs)},
        error = function(e){NULL}) })


    output$outcomeDisplayP <- renderText({
      req(outcomeData())
      dataPrintHelper(outcomeData(), 200)})


    ## RHS PLOTS ###

    output$distPlot <- renderPlot({
      tryCatch({
        req(distrConfig(),paramsTransformed())

        parser(distrConfig()$distrPlot)(
          paramsTransformed() %>%  as.matrix(),
          parser(distrConfig()$analyticDomain),
          parser(distrConfig()$analyticRange))},
        error = function(e){element_blank()})
    },
    height = 350, width = 'auto')

    observeEvent({input$distrID},{
      output$probHistPlot <- renderPlot({
        req(paramsTransformed())

        tryCatch({histogramMaker(
          (paramsTransformed() %>%  as.matrix())[,1],
          paste0("$",distrConfig()$intrParamTex, "$"))},
          error = function(e){ggplot2::element_blank()})
      }, height = 350, width = 'auto')

      output$probHistUI <-renderUI({
        if(distrConfig()$nVar > 1){
          #shinycssloaders::withSpinner(
          plotOutput(ns("probHistPlot"),inline = T)}
        else {div()}
      })

      output$probHistHelper <- renderUI({
        if(distrConfig()$nVar > 1) {helperMaker("Parameter Histogram",
                                                styleArg = "left:375px;")}
        else {div()}
      })


      output$ordinalPlot <- renderPlot({
        req(paramsTransformed())
        tryCatch(
          {orderedDistSpecialPlot(parser(distrConfig()$yStarPDF),paramsTransformed())},
          error = function(e){ggplot2::element_blank()}) },
        height = 350, width = 'auto')

      output$ordinalPlotUI <- renderUI(if(distrConfig()$distrGroup == "Ordered" ){
        #shinycssloaders::withSpinner(
          plotOutput(ns("ordinalPlot"), inline= T)
      } else {
        div()
      })

      output$ordinalHelper <- renderUI({
        if(
          distrConfig()$distrGroup == "Ordered"
          ){helperMaker("Ordinal Plot")}
        else {div()}})
    })

    output$marginalSelectorP <- renderUI({
      req(numX())
      marginalSelectInput(ns=ns,
                          choicesInput = paste0("X",1:(numX()-1)),
                          # pass ns into wrap whatever inputID given
                          inputID = "marginalSelectedP",
                          hidden = (distrConfig()$nVar == 1)) # hide for univariates
    })

observeEvent({
  probParams()
  input$marginalSelectedP
},{
  if(distrConfig()$nCovar > 1) {req(xVals())}
  testVals <- round(stats::rnorm(1, 2),5)
  if((parser(distrConfig()$transformFun))(testVals, xVals(), DGP = T) != testVals){
    margNumFFP <- substr(input$marginalSelectedP,2,2) %>%
      as.numeric()

    output$functionalFormPlot <- renderPlot({
      if(distrConfig()$distrGroup == "Ordered"){
        ffFun <- functionalFormPlotOrdered}
      else {ffFun <- functionalFormPlot}

      tryCatch({ ffFun(
        transformFun = parser(distrConfig()$transformFun),
        paramRange = parser(distrConfig()$chartDomain)(distrConfig()$nCovar)[[1]],
        paramTex = parser_vec(distrConfig()$paramList)[[margNumFFP]],
        intrParamTex = distrConfig()$intrParamTex,
        fixValues = probParams(),
        multi = (distrConfig()$nVar != 1),
        margNum = margNumFFP,
        xVals = xVals(),
        xChoice = xChoices(),
        funcRange = parser(distrConfig()$funcFormRange),
        pdfFun = parser(distrConfig()$pdfList))},
        error = function(e){ggplot2::element_blank()})
    },height = 'auto', width = 'auto' #350, 350
    )

    output$functionalFormPlotUI <- renderUI({
      #shinycssloaders::withSpinner(
      plotOutput(ns("functionalFormPlot"))
      })
    # TODO: why is this plot call such a nightmare


        output$functionalFormHelper <- renderUI({
          helperMaker("Functional Form", styleArg = "left:375px;")
          })

      } else{

        output$functionalFormUI  <- renderUI({div()})
        output$functionalFormHelper <- renderUI({div()})
      }

    }, ignoreNULL = F)


    return(list(distrConfig=distrConfig, outcomeData=outcomeData,
                xChoices=xChoices))


  })
}

## To be copied in the UI
# mod_dgp_tab_ui("dgp_tab_1")

## To be copied in the server
# mod_dgp_tab_server("dgp_tab_1")

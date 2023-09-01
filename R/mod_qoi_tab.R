#' qoi_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qoi_tab_ui <- function(id){
  ns <- NS(id)

  tabPanel(
    title = uiOutput(ns("simTitleOutput")),
    column(
      4,
      fluidRow(
        uiOutput(ns("simHeader"), style = "padding-bottom:5px"),
        column(
          12,
          uiOutput(ns("simParamLatex"),
                   style = "padding-left:15px; padding-bottom:10px;"),
          uiOutput(ns("simVcovLatex"),
                   style = "padding-left:15px;"),
          helperMaker("Estimates (Sim)")
        ),
      ),
      hr(),
      fluidRow(
        column(
          12,
          uiOutput(ns("pickQOIBox")),
          helperMaker("Quantity of Interest")
        ),
        column(
          12,
          uiOutput(ns("simSliders")),
          helperMaker("Chosen Covariate")
        )
      ),
      column(12,
             div(id = "simEstimationDiv",
                 uiOutput(ns("simEstimationLatex"))),
             div(id = "simFundamentalDiv",
                 uiOutput(ns("simFundamentalLatex"))),
             helperMaker("Estimation and Fundamental Uncertainty")
      ),
    ),
    column(6,
           column(12,
                  helperMaker("QOI Histogram"),
                  plotOutput(ns("QOIChart")),
                  title = "Distribution of the quantity of interest"),
           column(12,
                  uiOutput(ns("ffSimHelper")),
                  uiOutput(ns("ffSimUI")),
                  title = "Other X fixed at means, parameters at MLEs"),
           column(8,
                  offset = 4,
                  uiOutput(ns("marginalSelectorSim"))),
    ),
  )

}

#' qoi_tab Server Functions
#'
#' @noRd
mod_qoi_tab_server <- function(id, distrConfig,
                               assumedDistrConfig,
                               MLEResult,
                               numXAssumed,
                               assumedXVals){
  # confirm reactive values from dgp tab passed through
  stopifnot(is.reactive(distrConfig))
  stopifnot(is.reactive(assumedDistrConfig))
  stopifnot(is.reactive(MLEResult))
  stopifnot(is.reactive(numXAssumed))
  stopifnot(is.reactive(assumedXVals))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    session$allowReconnect("force")

    ########### tab title #############
    titleTextSim <- reactiveVal(
      div(icon("chevron-right"),
          tags$b("Quantities of Interest"),
          style = "color:#c59267;",
          helperMakerNavbar("Simulation (Disabled)")
      )
    )
    observeEvent(
      distrConfig()$distrList,
      {
        #TODO: fix to be less shit
        shinyjs::disable(selector=".navbar-nav > li:nth-child(4)")
        shinyjs::disable(selector=".navbar-nav > li:nth-child(4) a")
        titleTextSim(
          div(icon("chevron-right"),
              tags$b("Quantities of Interest"),
              style = "color:#c59267;",
              helperMakerNavbar("Simulation (Disabled)")
          ))
      })
    observeEvent(
      assumedDistrConfig()$distrList,
      {
        shinyjs::enable(selector=".navbar-nav > li:nth-child(4)")
        shinyjs::enable(selector=".navbar-nav > li:nth-child(4) a")

        titleTextSim(
          div(icon("chevron-right"),
              tags$b("Quantities of Interest"),
              style = "color:#ffffff",
              helperMakerNavbar("Simulation")
          ))
      })
    output$simTitleOutput <- renderUI({
      titleTextSim()
    })

    ########### UI #############
    # TODO: clean up error handling

    output$simHeader <- renderUI({
      tryCatch(MLEResult(), error = function(e){tags$b("Please Choose A Model",
                                                       style = "color:red")})
      if( (is.null(assumedDistrConfig()$distrList)) ||(is.null(MLEResult()))){
        tags$b("Please Choose A Model", style = "color:red")
      } else { tags$p(tags$b("From Model Tab"))}
    })

    output$simParamLatex <- renderUI({
      req(MLEResult())
      coeffLatex(assumedDistrConfig()$paramTex,
                 assumedDistrConfig()$reparamTex,
                 MLEResult()$paramHat ) })

    output$simVcovLatex <- renderUI({
      req(MLEResult())
      vCovLatex(parser_vec(assumedDistrConfig()$paramList), MLEResult()$paramVCov )})

    output$pickQOIBox <- renderUI({
      req(MLEResult())
      QOISwitcher(ns=ns,assumedDistrConfig(), selectedQOI)})

    output$simSliders  <-  renderUI({
      req(MLEResult())
      simMultiSliderFunction(ns=ns, numXAssumed()-1)})

    output$simEstimationLatex <-  renderUI({
      req(MLEResult())
      parser(assumedDistrConfig()$latexList)(
        type = "Estimation Uncertainty",
        paramTex = assumedDistrConfig()$paramTex,
        browserWidth=shinybrowser::get_width()
      )})

    output$marginalSelectorSim <- renderUI({
      req(MLEResult())
      if (assumedDistrConfig()$nVar > 1 && assumedDistrConfig()$distrGroup != "Ordered"){
        marginalSelectInput(ns=ns,
                            choicesInput = paste0("X",1:(numXAssumed()-1)),
                            inputID = "marginalSelectedSim")
      } else{marginalSelectInput(ns=ns,hidden = T)}})

    ########### computation #############

    simXVals <- reactive({
      c(1,inputsClean(input, "simX", numXAssumed()-1))
    })

    output$simFundamentalLatex <-  renderUI({
      req(MLEResult())
      if(assumedDistrConfig()$nCovar > 1) {req(simXVals())}
      parser(assumedDistrConfig()$latexList)(
        assumedDistrConfig()$distrList,
        type = "Fundamental Uncertainty",
        xValsSim = if(length(simXVals()) == 1){simXVals()} else simXVals()[2:length(simXVals())],
        paramTex = assumedDistrConfig()$paramTex,
        intrParamTex = assumedDistrConfig()$intrParamTex,
        browserWidth=shinybrowser::get_width()
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
      intrTildeCreator(paramTilde(),parser(assumedDistrConfig()$transformFun), simXVals())})
    yTilde<- reactive({
      req(intrTilde())
      yTildeCreator(intrTilde(),model = parser(assumedDistrConfig()$drawFun))})

    observeEvent({
      paramTilde()
      input$marginalSelectedSim
    },{
      testVals <- round(stats::rnorm(1, 2),5) #todo find a better test
      if(((parser(assumedDistrConfig()$transformFun))(testVals, assumedXVals()) != testVals) &
         #(assumedDistrConfig()$distrGroup != "Ordered" ) &
         (assumedDistrConfig()$nVar != 1)){
        output$functionalFormPlotSim <- renderPlot({
          functionalFormWithCI(transformFun = parser(assumedDistrConfig()$transformFun),
                               fixValuesX = simXVals(),
                               paramTildes = paramTilde(),
                               funcRange = parser(assumedDistrConfig()$funcFormRange),
                               margNum = substr(input$marginalSelectedSim,2,2) %>%  as.numeric(),
                               intrParamTex = assumedDistrConfig()$intrParamTex )}, height = 350)

        output$ffSimUI <- renderUI({plotOutput(ns("functionalFormPlotSim"))})
        output$ffSimHelper <- renderUI({helperMaker("Functional Form (Simulation)")})
      } else {
        output$ffSimUI <-  renderUI({div()})
        output$ffSimHelper <- renderUI({div()})
      }

    }, ignoreNULL = F)

    QOIOutputs <- reactive({
      req(yTilde())
      if(assumedDistrConfig()$nCovar > 1) {req(simXVals())}
      QOIVisualization(yTilde(), intrTilde(), assumedDistrConfig(),
                       input$QOIid, QOIDF)})

    output$QOIChart  <- renderPlot({
      req(QOIOutputs())})

  })
}

## To be copied in the UI
# mod_qoi_tab_ui("qoi_tab_1")

## To be copied in the server
# mod_qoi_tab_server("qoi_tab_1")

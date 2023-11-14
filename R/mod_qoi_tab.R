#' qoi_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @import rintrojs
mod_qoi_tab_ui <- function(id){
  ns <- NS(id)

  tabPanel(
    title = uiOutput(ns("simTitleOutput")),
    value ="QOI",
    ### Adding tutorial button
    fluidRow(
      column(12,
             div(
               id = "qoi_step1",
               actionButton(ns("help_qoi"), "Press for Tutorial Mode"),
              ),
             style = 'align-items: right; text-align: right; margin-right: 25px;'
      ),
    ),
    column(
      4,
      fluidRow(
        uiOutput(ns("simHeader"), style = "padding-bottom:5px"),
        column(
          12,
          div(
            id = "qoi_step2",
            fluidRow(
              uiOutput(ns("simParamLatex"),
                       style = "padding-left:15px; padding-bottom:10px;")
            ),
          ),
          div(
            id = "qoi_step3",
            fluidRow(
              uiOutput(ns("simVcovLatex"),
                       style = "padding-left:15px;")
            ),
          ),
          helperMaker("Estimates (Sim)")
        ),
      ),
      hr(),
      fluidRow(
        div(
          id = "qoi_step4",
          fluidRow(
            column(
              12,
              uiOutput(ns("pickQOIBox")),
              helperMaker("Quantity of Interest")
            )
          ),
        ),
        column(
          12,
          div(
            id = "qoi_step5",
            fluidRow(
              uiOutput(ns("simSliders")),
              ### Adding Chosen Covariate as UI output
              uiOutput(ns("chosenCovariateHelper"))
              # helperMaker("Chosen Covariate")
            ),
          )
        )
      ),
      column(12,
             div(
               id = "qoi_step6",
               fluidRow(
                 div(id = "simEstimationDiv",
                     uiOutput(ns("simEstimationLatex")))
               ),
              ),
             div(
               fluidRow(
                 id = "qoi_step7",
                 div(id = "simFundamentalDiv",
                     uiOutput(ns("simFundamentalLatex"))),
                 helperMaker("Estimation and Fundamental Uncertainty")
               ),
             )
      ),
    ),
    column(6,
           div(
             id = "qoi_step8",
             fluidRow(
               column(12,
                      div(
                        helperMaker("QOI Histogram",
                                    styleArg = "left:600px;"),
                        plotOutput(ns("QOIChart"), height=350, width=600),
                        title = "Distribution of the quantity of interest"),
                      ### Adding Latex here
                      uiOutput(ns("QOIChartLatex"), width=600, style = "text-align: left; padding-left: 270px")
               )
             ),
           ),
           div(
             id = "qoi_step9",
             fluidRow(
               column(12,
                      div(
                        uiOutput(ns("ffSimHelper")),
                        uiOutput(ns("ffSimUI")),
                        title = "Other X fixed at means, parameters at MLEs"),
                      uiOutput(ns("marginalSelectorSim"), style = "text-align: left; padding-left: 300px")
               )
             ),
           ),
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


    observeEvent(input$help_qoi,
                 introjs(session, options = list("nextLabel"="Next",
                                                 "prevLabel"="Back",
                                                 "skipLabel"="Exit",
                                                 steps = helptext()[tab == "QOI"]),
                         events = list("oncomplete"=I('alert("Congrats on finishing this tutorial!")')))
    )

    observeEvent(
      distrConfig()$distrList,
      {
        #TODO: fix
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
      ### Editing this out so that we can have selected covariate for Ordered?
      # if (assumedDistrConfig()$nVar > 1 && assumedDistrConfig()$distrGroup != "Ordered"){
      if (assumedDistrConfig()$nVar > 1){
        marginalSelectInput(ns=ns,
                            choicesInput = paste0("X",1:(numXAssumed()-1)),
                            inputID = "marginalSelectedSim")
      } else{marginalSelectInput(ns=ns,hidden = T)}})

    ### Making Chosen Covariate Helper only pop-up if assumed DGP (X)
    output$chosenCovariateHelper <- renderUI({
      if(assumedDistrConfig()$nCovar > 1) {helperMaker("Chosen Covariate"
      )}
      else {div()}
    })

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

        ### NEED TO DO IF(ORDINAL) FOR FFORDINAL

        output$functionalFormPlotSim <- renderPlot({
          ### IF Statement for Ordered DGPs here
          if(distrConfig()$distrGroup == "Ordered")
          {
            functionalFormPlotOrderedWithCI(
              transformFun = parser(assumedDistrConfig()$transformFun),
              fixValuesX = simXVals(),
              paramTildes = paramTilde(),
              funcRange = parser(assumedDistrConfig()$funcFormRange),
              margNum = substr(input$marginalSelectedSim,2,2) %>%  as.numeric(),
              intrParamTex = assumedDistrConfig()$intrParamTex,
              pdfFun = parser(distrConfig()$pdfList)
            )
          }
          else{
            functionalFormWithCI(transformFun = parser(assumedDistrConfig()$transformFun),
                                 fixValuesX = simXVals(),
                                 paramTildes = paramTilde(),
                                 funcRange = parser(assumedDistrConfig()$funcFormRange),
                                 margNum = substr(input$marginalSelectedSim,2,2) %>%  as.numeric(),
                                 intrParamTex = assumedDistrConfig()$intrParamTex )
          }
          }, height = 350)

        ### Adding a loading spinner
        output$ffSimUI <- renderUI({plotOutput(ns("functionalFormPlotSim"),
                                               height=350, width=600) %>% withSpinner(color=baseColor3)})
        output$ffSimHelper <- renderUI({helperMaker("Functional Form (Simulation)",
                                                    styleArg = "left:600px;")})
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

    output$QOIChartLatex <- renderUI({
      req(QOIOutputs())
      req(distrConfig())
      req(input$QOIid)
      if(input$QOIid == "Expected Values"){

        div(
          style = "white-space: nowrap; display: flex; justify-content: left; align-items: left; height: 40px; margin-top: -50px; margin-bottom: 10px;",
          HTML(katex_html(paste0(distrConfig()$simXAxis_param_new),
                                 preview = FALSE,
                                 output = "html"))
        )
      }
      else{
        div()
      }
    })

  })
}

## To be copied in the UI
# mod_qoi_tab_ui("qoi_tab_1")

## To be copied in the server
# mod_qoi_tab_server("qoi_tab_1")

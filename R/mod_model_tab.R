#' model UI Function
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
mod_model_tab_ui <- function(id){
  ns <- NS(id)
  shinybrowser::detect()

    tabPanel(
      title = uiOutput(ns("assumedDistrNameOutput")),
      value ="Likelihood",

      ### Adding tutorial button
      fluidRow(
        column(12,
               div(
                 id = "mod_step1",
                 actionButton(ns("help_mod"), "Press for Tutorial Mode")
               ),
               style = 'align-items: right; text-align: right; margin-right: 25px;'
        ),
      ),

      div(
        id = "mod_step2",
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
      ),
      hr(),
      fluidRow(
        column(
          5,
          div(
            id = "mod_step3",
            fluidRow(
              column(
                12,
                id = "assumedDistrSelectCol",
                uiOutput(ns("assumedDistrSelect")),
                helperMaker("Model Selection"))
            ),
          ),
          div(
            id = "mod_step4",
            fluidRow(
              column(
                12,
                reactOutput(ns("modal")),
                uiOutput(ns("assumedXChoiceDiv"),
                         style = "padding-left:15px;"),
                ### Bringing Helper Maker up as output
                uiOutput(ns("hypCovariateHelper"))
              )
            ),
          ),
        ), # depends on actual
        column(
          6, id = "guesstimateCol",
          style = "width:400px",
          div(
            id = "mod_step6",
            fluidRow(
              tags$p(tags$b("Guesstimate"),
                     style = paste0("padding-left: 15px; color:", baseColor2)),
              div(
                uiOutput(ns("paramByHandSlider")),
                style= "padding-left:15px;float:left;"),
              div(actionButton(ns("resetByHand"),
                               label = "Set to MLE",
                               title = "Set Guesstimates to MLE"),
                  style = "padding-left:30px;padding-bottom:10px;float:left;"),
              helperMaker("Guesstimate")
            ),
          )
        )
      ),
      fluidRow(
        column(
          5,
          div(
            id = "mod_step5",
            fluidRow(
              tags$p(tags$b("Statistical Model"),
                     style = "padding-left: 15px;"),
              column(
                12,
                id = "statModelRow",
                uiOutput(ns("statModel")),
                helperMaker("Statistical Model")
              )
            ),
          ),
          hr(),
          div(
            id = "mod_step8",
            fluidRow(
              tags$p(tags$b("Log Likelihood"),
                     style = "padding-left: 15px;"),
              column(
                12,
                id = "likelihoodRow",
                uiOutput(ns("likelihood")),
                helperMaker("Likelihood")
              )
            ),
          ),
          hr(),
          tags$p(tags$b("Maximum Likelihood Estimates")),
          fluidRow(
          column(
            12,
            id = "estimatesRow",
            div(
              id = "mod_step10",
              fluidRow(
                uiOutput(ns("MLEParamLatex"),
                         style = "float:left;padding-left:30px;padding-top:10px;")
              ),
             ),
            div(
              id = "mod_step11",
              fluidRow(
                uiOutput(ns("MLEVcovLatex"),
                         style = "float:left;padding-left:30px;padding-top:10px;")
              ),
            ),
            helperMaker("Estimates")
          ),
          #style = "padding-left:30px",
          )
        ),
        column(
          6,
            column(12,
                   div(
                     id = "mod_step7",
                   div(
                     plotOutput(ns("MLEByHandPlot"),
                                height = "300px", width="600px", inline = T),
                     title = "Guesstimate vs. Observed Data",
                     helperMaker("Guesstimate Plot",
                                 styleArg = "left:600px;")
                   ),
          ),
          ),
            fluidRow(
              column(
                12,
                div(
                  id = "mod_step9",
                div(
                  #plotOutput(ns("MLEPlot")),
                  plotOutput(ns("MLEPlot"), height = "300px",width="600px"),
                  #uiOutput(ns("MLEPlot")),
                  title = "Other Parameters fixed at MLEs",
                  helperMaker("Likelihood Plot",
                              styleArg = "left:600px;"),
                  uiOutput(ns("marginalSelectorLL"), style = "text-align: left; padding-left: 300px")
                ),
            ),
            ),
            ),
        ),
        column(
          6,
          offset = 5,
          #br(),
          column(
            12,
            div(
              id = "mod_step12",
              div(
                uiOutput(ns("ffPlotLLHelper")),
                uiOutput(ns("ffPlotLLUI")),
                title = "Other X fixed at means, parameters fixed at MLEs",
                uiOutput(ns("marginalSelectorLLF"), style = "text-align: left; padding-left: 300px")
              ),
            )
          ),
        )
      ),
      # fluidRow(
      #   column(
      #     6,
      #     offset = 5,
      #     #br(),
      #     column(
      #       12,
      #       uiOutput(ns("ffPlotLLHelper")),
      #       uiOutput(ns("ffPlotLLUI")),
      #       title = "Other X fixed at means, parameters fixed at MLEs"
      #     ),
      #
      #     column(8,
      #            offset = 4,
      #            uiOutput(ns("marginalSelectorLLF"))),
      #   ),
      #   style = "padding-left:15px;"
      #
      # )

    tags$link(rel = "stylesheet",
              type="text/css",
              href="custom.css"))


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

    session$allowReconnect("force")

    titleTextAssumed <- reactiveVal(
      div(
        icon("chevron-right"),
        tags$b("Model: ---"),
        style = "color:#c59267;",
        helperMakerNavbar(str = "Likelihood Inference (Disabled)")
      )
    )

    observeEvent(input$help_mod,
                 introjs(session, options = list("nextLabel"="Next",
                                                 "prevLabel"="Back",
                                                 "skipLabel"="Exit",
                                                 steps = helptext()[tab == "Likelihood"]),
                         events = list("oncomplete"=I('alert("Now you can move on to the Quantities of Interest Tab, where you can simulate values that you might be interested that come from the model you just created.")')))
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
            #tags$b("Model: ---"),
            tags$b("Model "),
            distrConfig()$distrID,
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
              #tags$b("Model: ---"),
              tags$b("Model "),
              distrConfig()$distrID,
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
        label = tags$p(tags$b("Assumed Distribution"),
                       style = "font-size:15px; !important"),
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

    modalVisible <- reactiveVal(FALSE)
    observeEvent(input$showModal, modalVisible(TRUE))
    observeEvent(input$hideModal, modalVisible(FALSE))
    output$modal <- renderReact({
      Modal(isOpen = modalVisible(),
            Stack(tokens = list(padding = "15px", childrenGap = "10px"),
                  div(style = list(display = "flex"),
                      Text("Covariate Choice Explanations", variant = "large"),
                      div(style = list(flexGrow = 1)),
                      IconButton.shinyInput(
                        ns("hideModal"),
                        iconProps = list(iconName = "Cancel")
                      ),
                  ),
                  div(
                    tags$embed(type="text/html", src="https://docs.google.com/spreadsheets/u/1/d/e/2PACX-1vQ2V3E8dCUkjef9qU85Li53f_rE9tBp5dvQCkuBLJOhaSnVfAH38_fD3827Ln2Pu09W60xSDQRkCm5l/pubhtml?gid=16261992&single=true&widget=true&headers=false", width="750", height="800"),
                    style='color:#999'

                  )
            )
      )
    })

    output$statModel <- renderUI({
      req(input$assumedDistrID)
      if(assumedDistrConfig()$nCovar > 1) {req(numXAssumed())}
      parser(assumedDistrConfig()$latexList)(
        nXValsAssumed = numXAssumed()-1, type = "Model",
        modelDF = assumedDistrConfig(),
        browserWidth=shinybrowser::get_width())})

    output$likelihood <- renderUI({
      req(input$assumedDistrID)
      parser(assumedDistrConfig()$latexList)(type = "Likelihood",
                                             modelDF = assumedDistrConfig(),
                                             browserWidth=shinybrowser::get_width())
    })

    ### Hypothesized Covariate Pop-Up only appears if DGP(X)
    output$hypCovariateHelper <- renderUI({
      if(assumedDistrConfig()$nCovar > 1) {helperMaker("Hypothesize a Covariate"
      )}
      else {div()}
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
        multiModel = (assumedDistrConfig()$nVar != 1))},
        height = 301, width=600)

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
            ffFunLL <- functionalFormPlotOrdered
            fixValuesInput <- byHandParams()
            fixValuesInput[length(fixValuesInput)] <- exp(byHandParams()[length(byHandParams())])
            }
          else {
            ffFunLL <- functionalFormPlot
            fixValuesInput <- byHandParams()
          }

          ffFunLL(
            transformFun = parser(assumedDistrConfig()$transformFun),
            paramRange = parser(assumedDistrConfig()$chartDomain)(assumedDistrConfig()$nCovar)[[1]],
            paramTex = parser_vec(assumedDistrConfig()$paramList)[[margNumLLF]],
            intrParamTex = assumedDistrConfig()$intrParamTex,
            fixValues = fixValuesInput,
            multi = (assumedDistrConfig()$nVar != 1),
            margNum = margNumLLF,
            xVals = assumedXVals(),
            xChoice = assumedXChoices(),
            funcRange = parser(assumedDistrConfig()$funcFormRange),
            pdfFun = parser(assumedDistrConfig()$pdfList))
          },
            #height = 350
          #, width = 'auto'
          )

        #TODO: how can this call be shorter tho

        output$ffPlotLLUI <- renderUI({plotOutput(outputId = ns("functionalFormPlotLL"),
                                                  #inline = T,
                                                  height=350, width=600)})
        output$ffPlotLLHelper <- renderUI({helperMaker("Functional Form (Model)",
                                                       styleArg = "left:600px;")})

      } else {
        output$ffPlotLLUI  <- renderUI({div()})
        output$ffPlotLLHelper <- renderUI({div()})
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

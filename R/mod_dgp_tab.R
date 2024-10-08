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
  shinybrowser::detect()


    tabPanel(
      title = uiOutput(ns('distrNameOutput')),
      value = "dgp",

      ### Adding tutorial button
      fluidRow(
        column(12,
               div(
                 id = "dgp_step1",
                 actionButton(ns("help"), "Press for Tutorial Mode", style = "color: white; background-color: #d16103;")
                 ),
               style = 'align-items: right; text-align: right; margin-right: 25px;'
               ),
      ),

      fluidRow(
        column(
          ### User selects which Data Generation Process to use
          12,
          div(
            div(
              id = "dgp_step2",
              selectInput(
                inputId = ns("distrID"),
                label = tags$p(
                  tags$b("Data Generation Process"),
                  style = "font-size:15px; !important"),
                choices = optGroups,
                selected = selectedDist,
                width = "250px"),
              helperMaker("DGP Choice", styleArg = "left:305px;"),
            ),
            class = "distrInput"),
        hr(),
        column(4, id = "sliders",
               fluidRow(
                 column(
                   ### Prints out the probability model that corresponds to
                   ### the DGP that the user selected
                   12,
                   div(
                     id = "dgp_step3",
                     uiOutput(ns("distrTex")),
                   ),
                   div(
                     id = "dgp_step4",
                     helperMaker("Probability Model")
                   ),

                 ),
                 column(
                   ### User selects the number of observations (n)
                   12,
                   div(
                     id = "dgp_step5",
                     uiOutput(ns("obsSlider")),
                     helperMaker("Observation Choice",
                                 styleArg = "left:305px;")
                     ),
                 ),
                 fluidRow(),
                 div(
                   id = "dgp_step6",
                   fluidRow(
                     column(
                       ### User selects the covariates (X)
                       12,
                       reactOutput(ns("modal")),
                       uiOutput( #TODO: toggle divs with removeUI
                         ns("xChoiceDiv"),
                         style = "padding-left:15px;"),
                       helperMaker("Covariates",
                                   styleArg = "left:320px;"),

                     )
                   ),
                 ),
                 fluidRow(),
                 ### Creates the parameter sliders that correspond to the
                 ### selected DGP
                 div(
                   div(
                     id = "dgp_step7",
                     fluidRow(
                       uiOutput(ns("paramSlider")),
                       style = 'margin-left: 10px;'
                     ),
                   ),
                 ),
                ),
               hr(),
               column(
                 ### Prints out the generated outcome variables (Y)
                 12,
                 div(
                   id = "dgp_step8",
                   fluidRow(
                     uiOutput(ns("dataHeader")),
                     div(
                       textOutput(ns("outcomeDisplayP")),
                       style= "padding-top:15px;padding-left:15px",
                       width = "50px"),
                     helperMaker("Randomly Generated Data"),
                   ),
                 ),
               ),

        ),
        column(
          6,
          ### Adding HTML HERE ----
          div(
            id = "dgp_step9",
            fluidRow(
              column(
                width = 6,
                fluidRow(
                  div(
                    style = "display: flex; align-items: center; height: 400px;",
                    column(
                      2,
                      div(),
                    ),
                    column(
                      1,
                      uiOutput(ns("analyticalPlotYAxis")),
                      style= "text-align: right;"
                    ),
                    column(
                      9,
                      div(
                        column(
                          ### Prints the conditional distribution of Y as a density plot
                          12,
                          plotOutput(ns("distPlot"),
                                     inline = T),
                          style = "margin-left: -40px",
                          title = "Conditional Distribution of Y",
                          helperMaker("Analytical Plot",
                                      styleArg = "left:600px;"
                          )
                        )
                      ),
                    ),
                  )
                )
              )
            ),
          ),
          ### ENDING HTML HERE ----
          br(),
          column(
            ### Prints the underlying variable plot for an ordinal variable
              12,
              ### Going to have to put this in the function for ordinal Plot to make sure
              ### That this does not show if there are no ordinal plots
              div(
                id = "dgp_step10",
                uiOutput(ns("ordinalPlotUI"), inline = T),
                title = "(Unobserved) Underlying Variable",
                uiOutput(ns("ordinalHelper")),
              ),
            ),
          br(),
          column(
            ### Prints the distribution of the intermediate parameter as
            ### a histogram plot
              12,
              div(
                id = "dgp_step11",
                uiOutput(ns("probHistUI"), inline = T),
                title = "Distribution of intermediate parameter",
                uiOutput(ns("probHistHelper")),
              ),
            ),
          column(
            ### Prints the functional form that relates the covariate (X) to
            ### the intermediate parameter as a line plot
              12,
              div(
                id = "dgp_step12",
                div(
                  uiOutput(ns("functionalFormPlotUI"), inline = T),
                  title = "Other X fixed at means, parameters at chosen values",
                  uiOutput(ns("functionalFormHelper")),
                  uiOutput(ns("marginalSelectorP"), style = "text-align: left; padding-left: 300px"),
                ),
              ),
            ),
  )
  )
    ),
  tags$link(rel = "stylesheet",
            type="text/css",
            href="custom.css"))
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

    observeEvent(input$help,
                 if(input$distrID %in% c("Bernoulli", "Exponential", "Log Normal", "Poisson", "Stylized Normal")){
                   introjs(session, options = list("nextLabel"="Next",
                                                   "prevLabel"="Back",
                                                   "skipLabel"="Exit",
                                                   steps = helptext()[tab == "dgp" & step %in% c(1, 2, 3, 4, 5, 7, 8, 9)]),
                           events = list("oncomplete"=I('alert("Now you can try the DGP tab by yourself! Try out a few different DGP models, change, their parameters, and see how the data changes. When you are done, move on to the Model Tab, where you get to create a statistical model that expresses the relationship between covariates and the data you just generated!")')))

                 }
                 else{
                   if(input$distrID %in% c("Bernoulli (Logit)", "Exponential (Exp)", "Poisson (Exp)")){
                     introjs(session, options = list("nextLabel"="Next",
                                                     "prevLabel"="Back",
                                                     "skipLabel"="Exit",
                                                     steps = helptext()[tab == "dgp" & step %in% c(1, 2, 3, 4, 5, 7, 8, 9, 12)]),
                             events = list("oncomplete"=I('alert("Now you can try the DGP tab by yourself! Try out a few different DGP models, change, their parameters, and see how the data changes. When you are done, move on to the Model Tab, where you get to create a statistical model that expresses the relationship between covariates and the data you just generated!")')))

                   }
                   else{
                     if(input$distrID %in% c("Bernoulli (Logit, X)", "Bernoulli (Probit, X)", "Stylized Normal (X)", "Normal (X)", "Log Normal (X)", "Poisson (Exp, X)", "Neg Binomial (X)", "Exponential (Exp, X)")){
                       introjs(session, options = list("nextLabel"="Next",
                                                       "prevLabel"="Back",
                                                       "skipLabel"="Exit",
                                                       steps = helptext()[tab == "dgp" & step %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12)]),
                               events = list("oncomplete"=I('alert("Now you can try the DGP tab by yourself! Try out a few different DGP models, change, their parameters, and see how the data changes. When you are done, move on to the Model Tab, where you get to create a statistical model that expresses the relationship between covariates and the data you just generated!")')))

                     }
                     else{
                       introjs(session, options = list("nextLabel"="Next",
                                                       "prevLabel"="Back",
                                                       "skipLabel"="Exit",
                                                       steps = helptext()[tab == "dgp"]),
                               events = list("oncomplete"=I('alert("Now you can try the DGP tab by yourself! Try out a few different DGP models, change, their parameters, and see how the data changes. When you are done, move on to the Model Tab, where you get to create a statistical model that expresses the relationship between covariates and the data you just generated!")')))

                     }
                   }
                 }
                 )


    # observeEvent(input$help,
    #              introjs(session, options = list("nextLabel"="Next",
    #                                              "prevLabel"="Back",
    #                                              "skipLabel"="Exit",
    #                                              steps = helptext()[tab == "dgp"]),
    #                      events = list("oncomplete"=I('alert("Now you can move on to the Model Tab, where you get to create a statistical model that expresses the relationship between covariates and the data you just generated!")')))
    # )


    output$distrNameOutput <- renderUI({
      div(id = "DGPTitle",
            tags$b("DGP: "), input$distrID,
          helperMakerNavbar(str = "DGPs and Probability"),
          title = "DGPs/Probability Tab"
      )
    })

    output$distrTex <- renderUI({
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
    height = 350, width = 600)

    ### Adding in y axis latex
    output$analyticalPlotYAxis <- renderUI({
      if(!is.na(distrConfig()$secondParamTex)){
        div(
          style = "transform: rotate(-90deg); white-space: nowrap; display: flex; justify-content: center; align-items: center; height: 400px;",
          tags$p(HTML(katex_html(paste0("P(y|", distrConfig()$intrParamTex, ", ", distrConfig()$secondParamTex, ")"),
                                 preview = FALSE,
                                 output = "html")),
                 style = "text-align: right;")
        )
      }
      else{
        div(
          style = "transform: rotate(-90deg); white-space: nowrap; display: flex; justify-content: center; align-items: center; height: 400px;",
          tags$p(HTML(katex_html(paste0("P(y|", distrConfig()$intrParamTex, ")"),
                                 preview = FALSE,
                                 output = "html")),
                 style = "text-align: right;")
        )
      }
    })

    observeEvent({input$distrID},{
      output$probHistPlot <- renderPlot({
        req(paramsTransformed())

        tryCatch({histogramMaker(
          (paramsTransformed() %>%  as.matrix())[,1],
          paste0("$",distrConfig()$intrParamTex, "$"))},
          error = function(e){ggplot2::element_blank()})
      }, height = 350, width = 600)

      output$probHistUI <-renderUI({
        if(distrConfig()$nVar > 1){
          #shinycssloaders::withSpinner(
          plotOutput(ns("probHistPlot"),inline = T)}
        else {div()}
      })

      output$probHistHelper <- renderUI({
        if(distrConfig()$nVar > 1) {helperMaker("Parameter Histogram",
                                                styleArg = "left:600px;"
                                                )}
        else {div()}
      })


      output$ordinalPlot <- renderPlot({
        req(paramsTransformed())
        tryCatch(
          {orderedDistSpecialPlot(parser(distrConfig()$yStarPDF),paramsTransformed())},
          error = function(e){ggplot2::element_blank()}) },
        height = 350, width = 600)

      output$ordinalPlotUI <- renderUI(if(distrConfig()$distrGroup == "Ordered" ){
        #shinycssloaders::withSpinner(
          plotOutput(ns("ordinalPlot"), inline= T)
      } else {
        div()
      })

      output$ordinalHelper <- renderUI({
        if(
          distrConfig()$distrGroup == "Ordered"
        ){helperMaker("Ordinal Plot",
                      styleArg = "left:600px;")}
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
    }#350, 350
    )

    output$functionalFormPlotUI <- renderUI({
      #shinycssloaders::withSpinner(
      plotOutput(ns("functionalFormPlot"),
                 height = 350, width = 600 )
      })
    # TODO: why is this plot call such a nightmare


        output$functionalFormHelper <- renderUI({
          helperMaker("Functional Form",
                      styleArg = "left:600px;"
                      )
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

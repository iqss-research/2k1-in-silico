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
  tutorialText <- read.csv(app_sys("tutorialText.csv"))
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
        # helperMakerNavbar(str = "DGPs and Probability"),
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
    if(distrConfig()$distrGroup != "Real"){obsSliderFun(distrConfig()$nVar)} else div() })


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
    browser()
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
    req(paramsTransformed())
    # parser(distrConfig()$distrPlot)(
    #   paramsTransformed() %>%  as.matrix(),
    #   parser(distrConfig()$analyticDomain),
    #   parser(distrConfig()$analyticRange))
    element_blank()
    },
    height = 350, width = 350)




}

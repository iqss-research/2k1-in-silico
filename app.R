
source("preamble.R")

#######################################################################



server <- function(input, output, session) {
    
    shinyjs::addClass(id = "tabs", class = "navbar-right")
    
    titleText <- reactiveVal("")
    
    output$distrNameOutput <- renderUI({titleText()})
    
    output$paramSlider <- renderUI({paramSwitcher(input$distrID)})

    output$outcomeDisplayP <- renderText({outTextP()})
    
    output$outcomeDisplayL  <- renderText({outTextL()})
    

    noDataStrP <- "!-----No Data Generated-----!"
    noDataStrL <- "!-----Generate Data on Probability Page-----!"
    
    outTextP <- reactiveVal(noDataStrP)
    outTextL <- reactiveVal(noDataStrL)
   
    observeEvent({input$distrID},{titleText(paste0(input$distrID, ": Probability"))})
    
    paramsToUse <- reactiveVal(c())
    distrChartNum <- reactiveVal(1)
    marginalChoices <- reactiveVal()
    margNumTop <- reactiveVal()
    MLEVars <- reactiveVal(list())
    yTilde <- reactiveVal()
    outcomeData <- reactiveVal()
    
    
    observeEvent({input$distrID},{
        
        marginalChoices(marginalsChoicesSwitcher(input$distrID))
        output$marginalSelector1 <-  renderUI({
            marginalSelectInput(nVarSwitcher(input$distrID), 1, marginalChoices())})
        output$marginalSelector2 <- renderUI({
            marginalSelectInput(nVarSwitcher(input$distrID), 2, marginalChoices())})
        removeUI(selector = '#xPrint')
        
    })
        
    observeEvent({input$xRow},{
        removeUI(selector = '#xPrint')
        insertUI(selector = '#placeholder',
            ui = tags$div(
                tags$p(paste0(c("X: ", sapply(
                    indepVarsBase[input$xRow %>%  as.integer(),1:nVarSwitcher(input$distrID)],
                    function(a){sprintf("%0.2f",a)})),collapse = ", ")),
                id = "xPrint", style = "padding-top:15px"),
            where = "afterEnd")
        
        })
    
    observeEvent({input$marginalSelected2}, 
                 {
                     margNumTop(which(marginalsChoicesSwitcher(input$distrID)== input$marginalSelected2))
                     MLEVars(MLEPlot(input$distrID, outcomeData(), margNumTop()))
                     })
    
    observeEvent({
        input$param1
        input$param2
        input$param3
        input$param4
        input$param5
        input$distrID
        input$nObs
        },{
        if(!is.null(input$param1) &&
           !is.null(eval(parse(text= paste0("input$param",(nVarSwitcher(input$distrID)))) )
        )){
            paramsToUse <- reactiveVal(c())
            listParser(nVarSwitcher(input$distrID), "paramsToUse( c(paramsToUse(), input$param?))", environment())
            
            output$distPlot <- renderPlot({try({
                distrPlot(input$distrID, paramsToUse(), input$xRow %>%  as.integer())}, silent = F)})
            
            outcomeData(drawSwitcher(input$distrID, param = paramsToUse(), nObs = input$nObs))
            
            outTextP(dataPrintSwitcher(input$distrID, "<b>Data</b>: ",outcomeData(), 200))
            outTextL(dataPrintSwitcher(input$distrID, "<b>Data from Probability Tab: </b>",outcomeData(), 200))
            
            MLEVars(MLEPlot(input$distrID, outcomeData(), margNumTop()))
            
            output$MLEPlot <- renderPlot({MLEVars()$plot })
            
            yTilde(yTildeCreator(paramHat = MLEVars()$paramHat,
                                 paramVCov =  MLEVars()$paramVCov,
                                 model = modelSwitcher(input$distrID),
                                 1000
                                 ))
            
            output$simHist <- renderPlot({simHist(yTilde())})
            
            output$QOITable <- renderDataTable({QOITables(yTilde(), "probGrt")})
        }
            
    })
    
    
    output$distr <- renderUI({latexSwitcher(input$distrID, type = "Distr")})
    
    output$statModel <- renderUI({latexSwitcher(input$distrID, type = "Model")})
    
    output$likelihood <- renderUI({latexSwitcher(input$distrID, type = "Likelihood")})


}


# Run the application 
shinyApp(ui = ui, server = server,
         onStart = function(){
             oldw <<- getOption("warn")
             options(warn = -1)
             onStop(function(){
                 options(warn = oldw)
                 
             })
             
         })


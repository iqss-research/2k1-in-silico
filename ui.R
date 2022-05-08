
dashboardLogo <- shinyDashboardLogoDIY(
  
  boldText = " in Silico:"
  ,mainText = ""
  ,textSize = 20
  ,badgeText = ""
  ,badgeTextColor = "white"
  ,badgeTextSize = 0
  ,badgeBackColor = iqOrangeStr
  ,badgeBorderRadius = 0
  
)



ui <- 
  navbarPage(
    introjsUI(),
    header = tags$head(tags$link(rel = "stylesheet", 
                                 type = "text/css",
                                 href = "styles.css"),
                       title=div(
                         img(src="2k1-logo-icon.png"),
                         tags$b("  in Silico"), class="titleDiv"),
    ),  
    windowTitle = " in Silico", 
    theme = bs_theme(
      version = 3,
      bootswatch = "yeti",
      primary = iqOrangeStr,
      "navbar-default-bg" = iqOrangeStr,
    ),
    selected = uiOutput("distrNameOutput"),
    tabPanel(
      title = "Introduction",
    ),
    tabPanel(
      title = uiOutput("distrNameOutput"),
      id = "Probability",
      shinyjs::useShinyjs(),
      withMathJax(),
      # uiOutput("popoversDGP"),
      fluidRow(
        column(
          4,
          div(
            id = "distrIDDiv", 
            selectInput(
              inputId = "distrID",
              label = tags$p(tags$b("Data Generation Process"),style = "font-size:15px; !important"),
              choices = optGroups , selected = selectedDist, 
              width = "250px"), class = "distrInput"),
        ), 
        column(
          width = 4, offset = 2,
        )
      ),
      hr(),
      column(4, id = "sliders",
             fluidRow(
               uiOutput("distrTex"),
               uiOutput("obsSlider"),
               uiOutput("xChoiceDiv", style = "padding-left:15px;"),
               uiOutput("paramSlider")
             ),
             hr(),
             fluidRow(uiOutput("dataHeader"),
                      div(htmlOutput("outcomeDisplayP"),
                          style= "padding-top:15px;padding-left:15px", width = "50px")),
      ),
      column(6,
             div(id = "distPlotDiv",plotOutput("distPlot", inline = T), title = "Conditional Distribution of Y"),
             hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
             div(plotOutput("specialPlot", inline = T), title = "(Unobserved) Underlying Variable"),
             hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
             div(id = "probHistPlotDiv", plotOutput("probHistPlot", inline = T), title = "Distribution of intermediate parameter"),
             hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
             div(id = "FFPlotDiv",plotOutput("functionalFormPlot", inline = T),title = "Other X fixed at means, parameters at chosen values"),
             uiOutput("marginalSelectorP", style = "padding-left:155px"),
             
      ),
    ),
    tabPanel(
      title = uiOutput("assumedDistrNameOutput"),
      id ="Likelihood",
      fluidRow(
        column(12,
               tags$p(tags$b("Generated Y (from Probability Tab)")),
               div(htmlOutput("outcomeDisplayL"), style= "padding-left:15px;")
        ),
        style = "padding-bottom:10px;"
      ),
      hr(),
      fluidRow(
        column(
          3,id = "assumedDistrSelectCol",
               uiOutput("assumedDistrSelect")), # depends on actual
        column(
          width = 4, offset = 5,
        )
      ),
      fluidRow(
        column(
          6,
          fluidRow(uiOutput("assumedXChoiceDiv", style = "padding-left:15px;")),
          fluidRow(id = "statModelRow", uiOutput("statModel")),
          hr(), 
          fluidRow(id = "likelihoodRow", uiOutput("likelihood")),
          hr(), 
          tags$p(tags$b("Maximum Likelihood Estimates")),
          fluidRow(id = "estimatesRow",
                   uiOutput("MLEParamLatex", style = "float:left;padding-left:30px;padding-top:10px;"),
                   uiOutput("MLEVcovLatex" , style = "float:left;padding-left:30px;padding-top:10px;")),
          style = "padding-left:30px",
        ),
        column(6, id = "guesstimateCol",
               tags$p(tags$b("Guesstimate"), style = paste0("color:", baseColor2)),
               div(uiOutput("paramByHandSlider"), style= "padding-left:15px;float:left;"),
               div(actionButton("resetByHand", label = "Set to MLE", title = "Set Guesstimates to MLE"),
                   style = "padding-left:30px;padding-bottom:10px;float:left;"),
               div(id = "byHandPlotDiv", plotOutput("MLEByHandPlot", height = "auto"), title = "Guesstimate vs. Observed Data"),
        )
      ),
      fluidRow(
        column(6, offset = 6, 
               div(id = "MLEPlotDiv", plotOutput("MLEPlot", height = "300px"), title = "Other Parameters fixed at MLEs"), 
               column(8,offset = 4,uiOutput("marginalSelectorLL")),
               hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
               div(id = "FFLPlotDiv", plotOutput("functionalFormPlotLL"), title = "Other X fixed at means, parameters fixed at MLEs"),
               column(8,offset = 4, uiOutput("marginalSelectorLLF")),
        ), 
        style = "padding-left:15px;"
      ),
    ),
    tabPanel(
      title =uiOutput("simTitleOutput"),
      fluidRow(
        column(
          width = 4, offset = 6,
          actionButton(
        ),
      ),
      column(4,
             fluidRow(
               uiOutput("simHeader", style = "padding-bottom:5px"),
               fluidRow(id = "simEstimatesRow",
               uiOutput("simParamLatex", style = "padding-left:15px; padding-bottom:10px;"),
               uiOutput("simVcovLatex", style = "padding-left:15px;")),
             ),
             hr(),
             fluidRow(
               div(id = "QOIPickerDiv", uiOutput("pickQOIBox")),
               div(id = "simXDiv", uiOutput("simSliders"))
             ),
             fluidRow(
               div(id = "simEstimationDiv", uiOutput("simEstimationLatex")),
               div(id = "simFundamentalDiv", uiOutput("simFundamentalLatex")),
             ),
             
      ),
      column(6,
             fluidRow(div(id = "QOIPlotDiv", plotOutput("QOIChart")), title = "Distribution of the quantity of interest"),
             fluidRow(
               div(id = "FFSimplotDiv", plotOutput("functionalFormPlotSim"), title = "Other X fixed at means, parameters at MLEs"),
               column(8,offset = 4, uiOutput("marginalSelectorSim")),
             ),
      ),
    ),
    tabPanel(HTML(" </a></li><li><a href=\'https://projects.iq.harvard.edu/2k1-in-silico/notation' target = '_blank'>About/Help</a>")
    ),
    id = "tabs"
    
  )

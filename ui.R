
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
    id = "tabs",
    header = tags$head(
      tags$link(rel = "stylesheet", 
                type = "text/css",
                href = "styles.css"),
      shinyjs::useShinyjs(),
      introjsUI(),
    ),
    title=div(
      img(src = "2k1-logo-icon.png", id = "shield", style = "cursor: pointer;"),
      tags$b("  in Silico"), class="titleDiv"),
    windowTitle = " in Silico", 
    theme = bs_theme(
      version = 3,
      bootswatch = "yeti",
      primary = iqOrangeStr,
      "navbar-default-bg" = iqOrangeStr,
    ),
    selected = icon("map"),#uiOutput("distrNameOutput"),
    tabPanel(
      title = icon("map"),
      id = "Introduction",
      uiOutput("introductoryText"),
    ),
    tabPanel(
      title = uiOutput("distrNameOutput"),
      withMathJax(),
      fluidRow(
        column(
          4,
          div(
            id = "distrIDDiv", 
            selectInput(
              inputId = "distrID",
              label = tags$p(
                tags$b("Data Generation Process"),
                style = "font-size:15px; !important"
              ),
              choices = optGroups, selected = selectedDist, 
              width = "250px"),
            class = "distrInput",
            helperMaker("DGP Choice"),
          ),
        ), 
      ),
      hr(),
      column(4, id = "sliders",
             fluidRow(
               column(
                 12,
                 uiOutput("distrTex"),
                 helperMaker("Probability Model")),
               column(
                 12,uiOutput("obsSlider"),
                 helperMaker("Observation Choice")),
               column(
                 12,
                 uiOutput( #TODO: toggle divs with removeUI
                   "xChoiceDiv",
                   style = "padding-left:15px;"),
                 helperMaker("Covariates")
               ),
               uiOutput("paramSlider")
             ),
             hr(),
             column(
               12,
               fluidRow(
                 uiOutput("dataHeader"),
                 div(
                   htmlOutput("outcomeDisplayP"),
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
          plotOutput("distPlot", inline = T), title = "Conditional Distribution of Y",
          helperMaker("Analytical Plot", styleArg = "left:375px;")),
        hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
        column(
          12,
          plotOutput("ordinalPlot", inline = T), title = "(Unobserved) Underlying Variable",
          uiOutput("ordinalHelper")
        ),
        hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
        column(
          12,
          plotOutput("probHistPlot", inline = T), title = "Distribution of intermediate parameter",
          helperMaker("Parameter Histogram", styleArg = "left:375px;")
        ),
        column(
          12,
          plotOutput("functionalFormPlot", inline = T),title = "Other X fixed at means, parameters at chosen values",
          helperMaker("Functional Form", styleArg = "left:375px;")
        ),
        uiOutput("marginalSelectorP", style = "padding-left:155px"),
        
      ),
    ),
    tabPanel(
      title = uiOutput("assumedDistrNameOutput"),
      value ="Likelihood",
      fluidRow(
        column(
          12,
          tags$p(tags$b("Generated Y (from Probability Tab)")),
          div(htmlOutput("outcomeDisplayL"),
              helperMaker("Data for Inference", styleArg = "left:350px"),
              style= "padding-left:15px;")
        ),
        style = "padding-bottom:10px;"
      ),
      hr(),
      fluidRow(
        column(
          3,id = "assumedDistrSelectCol",
          uiOutput("assumedDistrSelect"),
          helperMaker("Model Selection")
        ) # depends on actual
      ),
      fluidRow(
        column(
          5,
          column(
            12,
            uiOutput("assumedXChoiceDiv",
                     style = "padding-left:15px;"),
            helperMaker("Hypothesize a Covariate"),
          ),
          column(
            12,id = "statModelRow", uiOutput("statModel"),
            helperMaker("Statistical Model")),
          hr(), 
          column(
            12,id = "likelihoodRow", uiOutput("likelihood"),
            helperMaker("Likelihood")),
          hr(), 
          tags$p(tags$b("Maximum Likelihood Estimates")),
          column(
            12, id = "estimatesRow",
            uiOutput("MLEParamLatex", style = "float:left;padding-left:30px;padding-top:10px;"),
            uiOutput("MLEVcovLatex" , style = "float:left;padding-left:30px;padding-top:10px;"),
            helperMaker("Estimates")
          ),
          style = "padding-left:30px",
        ),
        column(
          6, id = "guesstimateCol",
          style = "width:400px",
          tags$p(tags$b("Guesstimate"), style = paste0("color:", baseColor2)),
          div(uiOutput("paramByHandSlider"), style= "padding-left:15px;float:left;"),
          div(actionButton("resetByHand", label = "Set to MLE", title = "Set Guesstimates to MLE"),
              style = "padding-left:30px;padding-bottom:10px;float:left;"),
          column(12, plotOutput("MLEByHandPlot", height = "auto"),
                 title = "Guesstimate vs. Observed Data", helperMaker("Guesstimate Plot")),
          helperMaker("Guesstimate")
        )
      ),
      fluidRow(
        column(
          6, offset = 5,
          column(
            12,
            plotOutput("MLEPlot", height = "300px"), title = "Other Parameters fixed at MLEs",
            helperMaker("Likelihood Plot")
          ), 
          column(8,offset = 4,uiOutput("marginalSelectorLL")),
          hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
          column(
            12,
            plotOutput("functionalFormPlotLL"),
            title = "Other X fixed at means, parameters fixed at MLEs",
            helperMaker("Functional Form (Model)")
          ),
          
          column(8,offset = 4, uiOutput("marginalSelectorLLF")),
        ), 
        style = "padding-left:15px;"
      ),
    ),
    tabPanel(
      title =uiOutput("simTitleOutput"),
      column(
        4,
        fluidRow(
          uiOutput("simHeader", style = "padding-bottom:5px"),
          column(
            12,
            uiOutput("simParamLatex", style = "padding-left:15px; padding-bottom:10px;"),
            uiOutput("simVcovLatex", style = "padding-left:15px;"),
            helperMaker("Estimates (Sim)")
          ),
        ),
        hr(),
        fluidRow(
          column(12, uiOutput("pickQOIBox"),
                 helperMaker("Quantity of Interest")),
          column(12, uiOutput("simSliders"),
                 helperMaker("Chosen Covariate"))
        ),
        column(12,
               div(id = "simEstimationDiv", uiOutput("simEstimationLatex")),
               div(id = "simFundamentalDiv", uiOutput("simFundamentalLatex")),
               helperMaker("Estimation and Fundamental Uncertainty")
        ),
      ),
      column(6,
             column(12, helperMaker("QOI Histogram"), plotOutput("QOIChart"), title = "Distribution of the quantity of interest"),
             column(12, helperMaker("Functional Form (Simulation)"), 
                    plotOutput("functionalFormPlotSim"), title = "Other X fixed at means, parameters at MLEs"),
             column(8,offset = 4, uiOutput("marginalSelectorSim")),
      ),
    ),
    tabPanel(HTML(" </a></li><li><a href=\'https://projects.iq.harvard.edu/2k1-in-silico/notation' target = '_blank'>About</a>")
    ),
  )

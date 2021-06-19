
ui <- navbarPage(
  title = "Probability and Likelihood",
  theme = bs_theme(
    version = 3,
    bootswatch = "yeti",
    primary = "#BF5803",
    "navbar-default-bg" = "#BF5803",
    
  ),
  
  tabPanel(
    title = "Probability",
    shinyjs::useShinyjs(),
    withMathJax(),
    fluidRow(
      column(4,
             selectInput(
               "distrID",
               "Select Distribution",
               c("Bernoulli")
             )
      ), column(6,
                uiOutput("distr", style = "padding-top:15px")
      )
    ),
    fluidRow(
      column(4,
             sliderInput("param",
                         "Set Parameter Pi:",
                         min = 0,
                         max = 1,
                         value = .3,
                         step = .1
             )
      ),
      
      column(6,
             h4("Visualized Distribution"),
             plotOutput("distPlot", height = "300px", width = "75%")
      )
    ),
    hr(),
    
    fluidRow(
      column(4,
             sliderInput("nObs",
                         "Number of Observations:",
                         min = 1,
                         max = 200,
                         value = 20,
                         step = 1),
             br(),
             div(style="display:inline-block; padding-bottom:10px",
                 actionButton(inputId = "generateDataButton",
                              label = "Generate Data",
                              icon("play-circle")
                 ),
                 bsTooltip(
                   "generateDataButton", 
                   "After defining distribution parameters, press here to generate data and display a sample",
                   placement = "bottom", 
                   trigger = "hover"
                 )
             ),
      ),
      column(6,
             textOutput("outcomeDisplay")
      )
    )
  ),
  tabPanel(
    title ="Likelihood",
    fluidRow(
      column(6,
             textOutput("outcomeDisplay2")
      ),
      style = "padding-bottom:10px"
    ),
    hr(),
    fluidRow(
      column(4,uiOutput("statModel"))
      
    ),
    fluidRow(
      column(4,
             uiOutput("likelihood")
      ),
      column(6,
             plotOutput("MLEPlot", height = "400px")
      )
    )
  ),
  id = "tabs"
  
)

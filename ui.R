
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
             selectInput(inputId = "distrID",label = "Select Distribution",
                         choices = c(
                           "Bernoulli", "Stylized Normal" , "Poisson", "Exponential", "Log-Normal"
                         ) , selected = "Log-Normal"
             )
      ), column(6,
                uiOutput("distr", style = "padding-top:15px")
      )
    ),
    fluidRow(
      column(4,uiOutput("paramSlider")),
      
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
             htmlOutput("outcomeDisplayP")
      )
    )
  ),
  tabPanel(
    title ="Likelihood",
    fluidRow(
      column(6,
             htmlOutput("outcomeDisplayL")
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


dashboardLogo <- shinyDashboardLogoDIY(
  
  boldText = "Teach in Silico:"
  ,mainText = ""
  ,textSize = 20
  ,badgeText = ""
  ,badgeTextColor = "white"
  ,badgeTextSize = 0
  ,badgeBackColor = "#BF5803"
  ,badgeBorderRadius = 0
  
)



ui <- 

  navbarPage(
  
  tags$head(tags$style(HTML("
    .titleDiv {
      position: relative;
      top: -10px;
    } 
                            "))),  
  title=div(img(src="2k1-logo-icon.png"), tags$b("  Teach in Silico"), class="titleDiv"),
  windowTitle = "Teach in Silico", 
  theme = bs_theme(
    version = 3,
    bootswatch = "yeti",
    primary = "#BF5803",
    "navbar-default-bg" = "#BF5803",
    
  ),
  selected = "Probability",
  
  tabPanel(
    title = "Probability",
    shinyjs::useShinyjs(),
    withMathJax(),
    fluidRow(
      column(4,
             selectInput(inputId = "distrID",label = "Select Distribution",
                         choices = distrList , selected = "Bernoulli-Logit"
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

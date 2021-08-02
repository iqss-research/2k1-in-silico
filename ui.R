
dashboardLogo <- shinyDashboardLogoDIY(
  
  boldText = " in Silico:"
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
  tags$script(src="js/index.js"),
  tags$head(tags$style(HTML("
    .titleDiv {
      position: relative;
      top: -10px;
    }
    
    select {
      font: inherit;
      letter-spacing: inherit;
      word-spacing: inherit;
    }
    
    .selectpicker:hover {
      border-color: #888;
    }
    
                            "))),  
  title=div(img(src="2k1-logo-icon.png"), tags$b("  in Silico"), class="titleDiv"),
  windowTitle = "2k1 in Silico", 
  theme = bs_theme(
    version = 3,
    bootswatch = "yeti",
    primary = "#BF5803",
    "navbar-default-bg" = "#BF5803",
    
  ),
  selected = uiOutput("distrNameOutput"),
  
  tabPanel(
    title = uiOutput("distrNameOutput"),
    id = "Probability",
    shinyjs::useShinyjs(),
    withMathJax(),
    fluidRow(
      column(4,
             selectInput(inputId = "distrID",label = "Select Distribution",
                         choices = optGroups , selected = selectedDist
             )
      ), column(6,
                uiOutput("distr", style = "padding-top:15px")
      )
    ),
    hr(),
    fluidRow(
      column(4, id = "sliders", uiOutput("paramSlider")),
      
      column(6,
             uiOutput("marginalSelector1"),
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
      column(4,
             fluidRow(uiOutput("statModel")),
             fluidRow(uiOutput("likelihood")),
      ),
      column(6,
             uiOutput("marginalSelector2"),
             plotOutput("MLEPlot", height = "400px")
      )
    )
  ),
  tabPanel(
    title ="Notation",
    value ="Notation",
  ),
  id = "tabs"
  
)

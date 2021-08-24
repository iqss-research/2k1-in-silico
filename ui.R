
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
    .navbar-nav {
      float: none !important;
    }
    .navbar-nav > li:nth-child(6) {
      float: right; !important
      right: 150px; !important
    }
    .navbar-nav > li:nth-child(7) {
      float: right;
    }

                            "))),  
    title=div(img(src="2k1-logo-icon.png"), tags$b("  in Silico"), class="titleDiv"),
    windowTitle = " in Silico", 
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
               plotOutput("distPlot", height = "400px", width = "100%")
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
      icon = icon("chevron-right"),
      fluidRow(
        column(6,
               htmlOutput("outcomeDisplayL")
        ),
        style = "padding-bottom:10px"
      ),
      hr(),
      fluidRow(
        column(5,
               fluidRow(uiOutput("statModel")),
               fluidRow(uiOutput("likelihood")),
               style = "padding-left:30px",
        ),
        column(6,
               column(6, uiOutput("marginalSelector2")),
               plotOutput("MLEPlot", height = "400px")
        )
      )
    ),
    tabPanel(
      title ="Simulation",
      icon = icon("chevron-right"),
      column(3,
             fluidRow(
               uiOutput("simParamLatex"),
               uiOutput("simVcovLatex"),
             ),
             fluidRow(
               selectInput(
                 inputId = "QOIid", label = div(tags$p("Quantity of Interest", style = "font-size:15px !important;")),
                 choices = QOIChoices, selected = selectedQOI, width = "200px"),
               uiOutput("simSliders")
             ),
             fluidRow(
               simMathJax1,
               uiOutput("simDynamicLatex"),
             ),
      ),
      column(6,
             fluidRow(plotOutput("QOIChart")),
      ),
    ),
    tabPanel(
      title ="About",
      value ="About",
      fluidRow(
        column(8,
               h2("2k1 in Silico"),
               h4("by",tags$a("Gary King", href="https://garyking.org"), "and", tags$a("Zagreb Mukerjee", href="https://zagrebmukerjee.com")),
               tags$p("This app illustrates major concepts from Gov2001 at Harvard University, the first course in the Harvard Government Department graduate methods sequence taught by Gary King."),
               tags$p("The course is open to all (even those not at Harvard) for credit, via the Harvard Extension School as Stat E-200. All the lectures and class materials, including this app, are available for other instructors to use in their courses as well. See the course website for more information:", tags$a("j.mp/G2001.", href= "https://j.mp/G2001")),
               tags$p("Code for this app is available on", tags$a("Github.", href= "https://github.com/iqss-research/probSimulator"))
        )
      )
    ),
    tabPanel(
      title ="Notation",
      value ="Notation",
      fluidRow(
        column(8, 
               tags$p("Notation largely follows slides for the class, available at ",tags$a("j.mp/G2001.", href= "https://j.mp/G2001")),
               fluidRow(notation1, style = "padding-bottom:10px; padding-left:30px"),
               fluidRow(notation2, style = "padding-bottom:10px; padding-left:30px"),
               fluidRow(notation3, style = "padding-bottom:10px; padding-left:30px"),
               fluidRow(notation4, style = "padding-bottom:10px; padding-left:30px"),
               fluidRow(notation5, style = "padding-bottom:10px; padding-left:30px"),
               fluidRow(notation6, style = "padding-bottom:10px; padding-left:30px"),
               fluidRow(notation7, style = "padding-bottom:10px; padding-left:30px"),
        )
      ),
    ),
    id = "tabs"
    
  )

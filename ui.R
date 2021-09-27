
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
    # TODO: make a stylesheet
    tags$head(tags$style(HTML("
    /* http://meyerweb.com/eric/tools/css/reset/ 
   v2.0 | 20110126
   License: none (public domain)
    */
    
    html, body, div, span, applet, object, iframe,
    h1, h2, h3, h4, h5, h6, p, blockquote, pre,
    a, abbr, acronym, address, big, cite, code,
    del, dfn, em, img, ins, kbd, q, s, samp,
    small, strike, strong, sub, sup, tt, var,
    b, u, i, center,
    dl, dt, dd, ol, ul, li,
    fieldset, form, label, legend,
    table, caption, tbody, tfoot, thead, tr, th, td,
    article, aside, canvas, details, embed, 
    figure, figcaption, footer, header, hgroup, 
    menu, nav, output, ruby, section, summary,
    time, mark, audio, video {
    	/*margin: 0;*/
    	padding: 0;
    	border: 0;
    	/*font-size: 100%;*/
    	/*vertical-align: baseline;*/
    }
    /* END OF RESET */
    
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
    .simInput .selectize-control {
      padding-left: 30px; !important
    }
    .distrInput .selectize-control {
      padding-left: 30px; !important
    }"))),  
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
        column(4,div(selectInput(
          inputId = "distrID",
          label = tags$p(tags$b("Data Generation Process"),style = "font-size:15px; !important"),
          choices = optGroups , selected = selectedDist, 
          width = "200px"), class = "distrInput"),
        ), 
      ),
      hr(),
      fluidRow(
        column(6, id = "sliders",
               tags$p(tags$b("Probability Model")),
               uiOutput("distrTex", style = "padding-top:15px"),
               tags$p(tags$b("Parameters"), style = "padding-top:15px"),
               uiOutput("obsSlider"),
               uiOutput("xChoiceDiv", style = "padding-left:15px;"),
               uiOutput("paramSlider", style= "padding-left:15px;"),
        ),
        column(6,
               plotOutput("distPlot", height = "350px", width = "350px"),
               plotOutput("probHistPlot", inline = T)
        )
      ),
      hr(),
      
      fluidRow(
        column(6,
               tags$p(tags$b("Data Generation of Y")),
               br(),
               
        ),
        column(6,div(htmlOutput("outcomeDisplayP"),
                     style= "padding-top:30px;padding-bottom:30px")
        )
      ),
    ),
    tabPanel(
      title = uiOutput("assumedDistrNameOutput"),
      id ="Likelihood",
      fluidRow(
        column(6,
               tags$p(tags$b("Generated Y (from Probability Tab)")),
               div(htmlOutput("outcomeDisplayL"), style= "padding-left:15px;")
        ),
        style = "padding-bottom:10px;"
      ),
      hr(),
      fluidRow(
        column(6, uiOutput("assumedDistrSelect")), # depends on actual
        
      ),
      fluidRow(
        column(6,
               uiOutput("assumedXChoiceDiv", style = "padding-left:15px;"),
               fluidRow(uiOutput("statModel")),
               style = "padding-left:30px",
        ),
        column(6,
               tags$p(tags$b("Guesstimate"), style = "color:#BF5803"),
               div(uiOutput("paramByHandSlider"), style= "padding-left:15px;float:left;"),
               div(actionButton("resetByHand", label = "Set to MLE"),
                   style = "padding-left:30px;padding-bottom:10px;float:left;"),
               div(plotOutput("MLEByHandPlot", height = "auto")),
               
        )
      ),
      fluidRow(
        column(6, fluidRow(uiOutput("likelihood"))),
        column(6, plotOutput("MLEPlot", height = "300px"),
               uiOutput("marginalSelector2", style = "padding-left:45px"),
               tags$p(tags$b("Maximum Likelihood Estimates")),
               fluidRow(
                 uiOutput("MLEParamLatex", style = "float:left;padding-left:30px;"),
                 uiOutput("MLEVcovLatex" , style = "float:left;padding-left:30px;"),
                 style = "padding-left:30px;"
               )), 
        style = "padding-left:15px;"
      )
    ),
    tabPanel(
      title ="Quantities of Interest",
      icon = icon("chevron-right"),
      column(6,
             fluidRow(
               tags$p(tags$b("From Likelihood Tab"), style = "padding-bottom:5px"),
               uiOutput("simParamLatex", style = "padding-left:15px; padding-bottom:10px;"),
               uiOutput("simVcovLatex", style = "padding-left:15px;"),
             ),
             fluidRow(
               uiOutput("pickQOIBox"),
               uiOutput("simSliders")
             ),
             fluidRow(
               uiOutput("simEstimationLatex"),
               uiOutput("simFundamentalLatex"),
             ),
      ),
      column(6,
             fluidRow(plotOutput("QOIChart")),
      ),
    ),
    tabPanel(
      title ="About/Help",
      value ="About",
      fluidRow(
        column(8,
               h2("2k1 in Silico"),
               h4("by",tags$a("Gary King", href="https://garyking.org"), "and", tags$a("Zagreb Mukerjee", href="https://zagrebmukerjee.com")),
               tags$p("This app illustrates major concepts from Gov2001 at Harvard University, the first course in the Harvard Government Department graduate methods sequence taught by Gary King."),
               tags$p("The course is open to all (even those not at Harvard) for credit, via the Harvard Extension School as Stat E-200. All the lectures and class materials, including this app, are available for other instructors to use in their courses as well. See the course website for more information:", tags$a("j.mp/G2001.", href= "https://j.mp/G2001")),
               tags$p("Documentation for this app is on", tags$a("our website.", href= "https://projects.iq.harvard.edu/2k1-in-silico/notation")),
               tags$p("Code for this app is available on", tags$a("Github.", href= "https://github.com/iqss-research/probSimulator"))
        )
      )
    ),
    id = "tabs"
    
  )

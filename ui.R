
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
    .navbar-nav > li:nth-child(5) {
      float: right; !important
      right: 150px; !important
    }
    .navbar-nav > li:nth-child(6) {
      float: right;
    }
    .simInput .selectize-control {
      padding-left: 30px; !important
    }
    .distrInput .selectize-control {
      padding-left: 30px; !important
    }")),
              # tags$script(HTML(js1)),
    ),  
    title=div(img(src="2k1-logo-icon.png"), tags$b("  in Silico"), class="titleDiv"),
    windowTitle = " in Silico", 
    theme = bs_theme(
      version = 3,
      bootswatch = "yeti",
      primary = iqOrangeStr,
      "navbar-default-bg" = iqOrangeStr,
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
          width = "250px"), class = "distrInput"),
        ), 
      ),
      hr(),
      fluidRow(
        column(12,dataTableOutput("realDataTable")),
      ),
      column(4, id = "sliders",
             fluidRow(uiOutput("distrTex"),
                      uiOutput("obsSlider"),
                      uiOutput("xChoiceDiv", style = "padding-left:15px;"),
                      uiOutput("paramSlider")),
             hr(),
             fluidRow(uiOutput("dataHeader"),
                      div(htmlOutput("outcomeDisplayP"),
                          style= "padding-top:15px;padding-left:15px", width = "50px")),
      ),
      column(6,
             div(plotOutput("distPlot", inline = T), title = "Conditional Distribution of Y"),
             hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
             div(plotOutput("specialPlot", inline = T), title = "(Unobserved) Underlying Variable"),
             hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
             div(plotOutput("probHistPlot", inline = T), title = "Distribution of intermediate parameter"),
             hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
             div(plotOutput("functionalFormPlot", inline = T),title = "Other X fixed at means, parameters at chosen values"),
                 #            hover = hoverOpts("ffplot_hover", delay = 100, delayType = "debounce")),
                 # uiOutput("ffhover_info"), style = "position:relative"),
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
        column(6, uiOutput("assumedDistrSelect")), # depends on actual
        
      ),
      fluidRow(
        column(6,
               uiOutput("assumedXChoiceDiv", style = "padding-left:15px;"),
               fluidRow(uiOutput("statModel")),
               hr(), 
               fluidRow(uiOutput("likelihood")),
               hr(), 
               tags$p(tags$b("Maximum Likelihood Estimates")),
               fluidRow(
                 uiOutput("MLEParamLatex", style = "float:left;padding-left:30px;padding-top:10px;"),
                 uiOutput("MLEVcovLatex" , style = "float:left;padding-left:30px;padding-top:10px;")),
               style = "padding-left:30px",
        ),
        column(6,
               tags$p(tags$b("Guesstimate"), style = paste0("color:", baseColor2)),
               div(uiOutput("paramByHandSlider"), style= "padding-left:15px;float:left;"),
               div(actionButton("resetByHand", label = "Set to MLE", title = "Reset Guesstimates to MLE"),
                   style = "padding-left:30px;padding-bottom:10px;float:left;"),
               div(plotOutput("MLEByHandPlot", height = "auto"), title = "Guesstimate vs. Observed Data"),
        )
      ),
      fluidRow(
        column(6, offset = 6, 
               div(plotOutput("MLEPlot", height = "300px"), title = "Other Parameters fixed at MLEs"), 
                              # hover = hoverOpts("MLEplot_hover", delay = 100, delayType = "debounce")),
                   # uiOutput("MLEhover_info"), style = "position:relative"),
               column(8,offset = 4,uiOutput("marginalSelectorLL")),
               hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
               div(plotOutput("functionalFormPlotLL"), title = "Other X fixed at means, parameters fixed at MLEs"),
                   #            hover = hoverOpts("ffLplot_hover", delay = 100, delayType = "debounce")),
                   # uiOutput("ffLhover_info"), style = "position:relative"),
               column(8,offset = 4, uiOutput("marginalSelectorLLF")),
        ), 
        style = "padding-left:15px;"
      ),
    ),
    tabPanel(
      title =uiOutput("simTitleOutput"),
      column(4,
             fluidRow(
               uiOutput("simHeader", style = "padding-bottom:5px"),
               uiOutput("simParamLatex", style = "padding-left:15px; padding-bottom:10px;"),
               uiOutput("simVcovLatex", style = "padding-left:15px;"),
             ),
             hr(),
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
             fluidRow(
               div(plotOutput("functionalFormPlotSim"), title = "Other X fixed at means, parameters at MLEs"),
                              # hover = hoverOpts("SimPlot_hover", delay = 100, delayType = "debounce")),
                   # uiOutput("SimHover_info"), style = "position:relative"),
               column(8,offset = 4, uiOutput("marginalSelectorSim")),
             ),
      ),
    ),
    tabPanel(HTML(" </a></li><li><a href=\'https://projects.iq.harvard.edu/2k1-in-silico' target = '_blank'>About/Help</a>")
    #   title ="About/Help",
    #   value ="About",
    #   fluidRow(
    #     column(8,
    #            h2("2k1 in Silico"),
    #            h4("by",tags$u(tags$a("Gary King", href="https://garyking.org")), "and", tags$u(tags$a("Zagreb Mukerjee", href="https://zagrebmukerjee.com"))),
    #            tags$p(tags$u(tags$a("Documentation", href="https://projects.iq.harvard.edu/2k1-in-silico/notation")), ", ",tags$u(tags$a("Website", href = "https://projects.iq.harvard.edu/2k1-in-silico" )),", ", tags$u(tags$a("Open Source Code", href= "https://github.com/iqss-research/probSimulator"))),
    #            tags$p(tags$b("2k1 in Silico"), "illustrates major concepts from Gov2001, taught by Gary King at Harvard University. Gov2001 is the first course in the Harvard Government Department's graduate methods sequence"),
    #            tags$p("The course is open to all (even those not at Harvard) for credit, via the Harvard Extension School as Stat E-200.  All the lectures and class materials, including this app, are available for other instructors to use in their courses or students to use for self-study. See the course website for more information:", tags$a("j.mp/G2001.", href= "https://j.mp/G2001")),
    #     )
    #   )
    ),
    id = "tabs"
    
  )

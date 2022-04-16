
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
    .navbar-nav > li:nth-child(7) {
      float: right; !important
      right: 150px; !important
    }
    .navbar-nav > li:nth-child(8) {
      float: right;
    }
    .simInput .selectize-control {
      padding-left: 30px; !important
    }
    .distrInput .selectize-control {
      padding-left: 30px; !important
    }")),
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
      title = "Introduction",
      tags$p(
        "The core enterprise of social science is to look at a set of",
        tags$i("data"),
        "- something we can see in the world - and try to learn about the",
        tags$i("data generating process"),
        "that created it. Maybe we want to know the size of the incumbency advantage?",
        "We have to estimate it from looking at a lot of elections.",
        "Or maybe we want to know how education is associated with income.",
        "Then we could look at a large number of peoples' incomes and educational attainment. We will show you some tools to think about this kind of problem."),
      actionButton("nextSlide", "Next"),
      hr(),
      conditionalPanel(
        "output.hideSlide1",
        tags$p(
          "The most fundamental tool we have is",
          tags$i("probability"),
          ", a conceptual framework that lets us talk about DGPs.",
          " A very simple data generating process is flipping an unfair coin.",
          "Use the slider to set how often the coin flips (H)eads,",
          "and then click the button to flip the coin 100 times. "
        ),
        sliderInput(
          inputId = "coinBias",
          label = "Coin Bias",
          min = 0, max = 1, value = .7,step = .05),
        actionButton(inputId = "flipCoin", label = "Flip Coin"),
        uiOutput("coinOutput"),
        tags$p("In this data generation process, you can see everything of relevance!",
               "You see the parameter - how biased the coin is - and the distribution",
               " - turning that value for bias into a series of flips."),
        actionButton("prevSlide1", "Back"),
        actionButton("nextSlide1", "Next"),
        hr()
      ),
      conditionalPanel(
        "output.hideSlide2",
        tags$p(
          "One important system of inference is Likelihood Inference.",
          "In this process, we look at a data set. We assume a family of models",
          " - do I want to represent this with a coin flip,",
          " the roll of a die, a normal distribution? ",
          "Then, given that assumption, we ask: what parameters make our ",
          "data most likely to appear?"),
        tags$p(
          "Now, take a look at a different set of coin flips."),
        div(div(
          tags$p("T T T H H T T H H H T H H H H T T T T T H T H H T T H T T T H H T T T T T H T T T T H H H H T T H T T H H H T T T T T T H H T H T T T T T T T T T T T T T T H T T T T H T H T T H T T T H H T T T H T T"),
          tags$p("Total Heads: 34; Total Tails: 66"),
          style = "padding-top:15px; padding-bottom:15px;padding-left:15px;
        background-color:#E7E9EB;"
        ), style = "padding-top:15px; padding-bottom:15px;"
        ),
        tags$p(
          " This time, you don't know the DGP: you have to guess it.",
          " Use the slider to try different values of bias - different",
          " models of the coin that made this data. The green layer ",
          "represents what you'd expect to see, if your guess for bias was correct.",
          " If the coin was 90 percent biased towards tails, would you expect",
          " to see this data? What about if it was completely fair?"),
        sliderInput(
          inputId = "coinBiasByHand",
          label = "Guesstimate Coin Bias",
          min = 0, max = 1, value = .7,step = .05),
        fluidRow(column(
          6,
          div(plotOutput("coinGuesstimate"),
              title = "Guesstimate vs. Observed Data"),
        )),
        tags$p("This is the key concept of likelihood inference:",
               " it's a systematic version of what you're doing here.",
               " We look at every possible value of the parameter - the bias",
               " of the coin - and ask which one is most compatible with",
               " the data we've seen."),
        actionButton("prevSlide2", "Back"),
        actionButton("nextSlide2", "Next"),
        hr()
      ),
      conditionalPanel(
        condition = "output.hideSlide3",
        tags$p(
          "Once we have this core idea of likelihood inference,",
          " we can extend it to a wide range of social science questions.",
          " Suppose I want to take 20 male people from a town, measure how tall they are,",
          " and learn what I can about the heights of the other men in town"),
        tags$p(
          "Empirical studies of many populations show that heights",
          " tend to be normally distributed. So I can start by assuming ",
          "this towns' heights are as well. Then to best understand the",
          " townspeople, I can ask - of all the normal distributions,",
          " with different means and standard deviations, which one makes",
          " the most sense with this data?"),
        tags$p(
          "Take a look at this histogram of heights.",
          " Then use these two sliders",
          " to change between different normal distributions.",
          " Which one looks closest to the data?"),
        sliderInput(
          inputId = "avgHt",
          label = "Average Height (inches)",
          min = 60, max = 80, value = 65,step = 1),
        sliderInput(
          inputId = "stdvHt",
          label = "Standard Deviation (inches)",
          min = .25, max = 5, value = 1,step = .25),
        fluidRow(column(
          6,
          div(plotOutput("htGuesstimate"),
              title = "Guesstimate vs. Observed Data"),
        )),
        actionButton("prevSlide3", "Back"),
        hr()
      ),
    ),
    tabPanel(
      title = uiOutput("distrNameOutput"),
      id = "Probability",
      shinyjs::useShinyjs(),
      withMathJax(),
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
          width = 4, offset = 4,
          actionButton(inputId = "dgpIntro", label = "Guide to DGPs")
        )
      ),
      hr(),
      # fluidRow(
      #   column(12,dataTableOutput("realDataTable")),
      # ),
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
               column(8,offset = 4,uiOutput("marginalSelectorLL")),
               hr(style = "visibility:hidden"), #TODO: find a better way to force linebreak
               div(plotOutput("functionalFormPlotLL"), title = "Other X fixed at means, parameters fixed at MLEs"),
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
               column(8,offset = 4, uiOutput("marginalSelectorSim")),
             ),
      ),
    ),
    tabPanel(HTML(" </a></li><li><a href=\'https://projects.iq.harvard.edu/2k1-in-silico/notation' target = '_blank'>About/Help</a>")
    ),
    id = "tabs"
    
  )

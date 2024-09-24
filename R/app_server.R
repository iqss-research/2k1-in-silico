#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  session$allowReconnect("force")

  observeEvent(input$highlightdgp, {
    updateNavbarPage(session = session,
                     inputId="tabs",
                     selected="Introduction")
  })

  observeEvent(input$btn,
    {
    runjs("ReactDOM.render(<TooltipBasicExampleWrapper />, document.getElementById('tooltip-content'));")

  # modalDialog(
  #   textInput("click-dgp", "Select DGP to get started!"),
  #   footer = modalButton("Close"),
  #   )
  })

  # modalVisible <- reactiveVal(FALSE)
  # observeEvent(input$showModal, modalVisible(TRUE))
  # observeEvent(input$hideModal, modalVisible(FALSE))
  # output$modal <- renderReact({
  #   Modal(isOpen = modalVisible(),
  #         Stack(tokens = list(padding = "15px", childrenGap = "10px"),
  #               div(style = list(display = "flex"),
  #                   Text("DGPs and Probability", variant = "large"),
  #                   div(style = list(flexGrow = 1)),
  #                   IconButton.shinyInput(
  #                     "hideModal",
  #                     iconProps = list(iconName = "Cancel")
  #                     ),
  #                 ),
  #                 div(
  #                   tags$p("Use this tab first"),
  #                   tags$p(HTML("On this tab, you can use the Probability Model to set
  #                                 up a Data Generating Process,
  #                                 change its parameters, and see how it reacts. Using this
  #                                 tab will help you develop an intuition for probability distributions,
  #                                 and how they can represent uncertain reality.")),
  #                   tags$p(tags$a(target='_blank', href='https://www.youtube.com/watch?v=6C7yRBfh2ok','This lecture video'),
  #                       " gives an in-depth overview of probability concepts."),
  #                   style='color:#999'
  #                   #tags$a(target="_blank", href="https://www.microsoft.com", "Microsoft")
  #                 )
  #           )
  #     )
  #   })

  observeEvent(input$btn, {
    runjs('var today_var = new Date(); alert(today_var); Shiny.onInputChange("today_var");')
  })

  observeEvent(input$help_intro,
               introjs(session, options = list("nextLabel"="Next",
                                                 "prevLabel"="Back",
                                                 "skipLabel"="Exit",
                                                 steps = helptext()[tab == "Intro" & step %in% c(1, 2, 3, 4, 5, 6)]),
                         events = list("oncomplete"=I('alert("Follow the arrow, and start with the DGP tab. Once there, start the DGP tutorial. Remember, you can always click on the question icons if you feel stuck and want a reminder about how each section fits into the big picture.")')))
                 )

  output$dgp_arrow <- renderUI({
    tryCatch(
      {
        dgp_out$outcomeData()
        uiOutput("dgp_arrow_noshow")

      },
      error=function(cond) {
        uiOutput("dgp_arrow_show")
      }
    )
  })

  output$dgp_arrow_show <- renderUI({
    tags$div(class="glyphicon glyphicon-chevron-up bounce")
  })

  output$dgp_arrow_noshow <- renderUI({
    tags$div()
  })


  output$buttonlink <- renderUI({
    tags$div(id='content')
  })

  output$browserwidth <- renderText({
    paste("width:",shinybrowser::get_width())
  })

  shinyjs::onclick("shield",
                   updateNavbarPage(session,
                                    "tabs",
                                    selected="Introduction"))


  dgp_out <- mod_dgp_tab_server("dgp_tab_1")

  model_out <- mod_model_tab_server("model_tab_1", dgp_out$distrConfig,
                       dgp_out$outcomeData, dgp_out$xChoices)

  mod_qoi_tab_server("qoi_tab_1", dgp_out$distrConfig,
                     model_out$assumedDistrConfig,
                     model_out$MLEResult,
                     model_out$numXAssumed,
                     model_out$assumedXVals)

}

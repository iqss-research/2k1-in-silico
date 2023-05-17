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

  observeEvent(
    input$tabs,

  modalDialog(
    textInput("click-dgp", "Select DGP to get started!"),
    footer = modalButton("Close"),
    )
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


  shinyjs::onclick("shield",
                   updateNavbarPage(session,
                                    "tabs",
                                    selected="Introduction"))

  # observe({
  #   print(input$tabs)
  #   print(input$tabs == "Introduction")
  # })

  dgp_out <- mod_dgp_tab_server("dgp_tab_1")

  model_out <- mod_model_tab_server("model_tab_1", dgp_out$distrConfig,
                       dgp_out$outcomeData, dgp_out$xChoices)

  mod_qoi_tab_server("qoi_tab_1", dgp_out$distrConfig,
                     model_out$assumedDistrConfig,
                     model_out$MLEResult,
                     model_out$numXAssumed,
                     model_out$assumedXVals)

}

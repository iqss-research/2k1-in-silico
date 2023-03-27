#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  observeEvent(input$gotodgp, {
    show_getStarted == FALSE
    updateNavbarPage(session = session,
                     inputId="tabs",
                     selected="dgp")
  })

  dgp_out <- mod_dgp_tab_server("dgp_tab_1")

  model_out <- mod_model_tab_server("model_tab_1", dgp_out$distrConfig,
                       dgp_out$outcomeData, dgp_out$xChoices)

  mod_qoi_tab_server("qoi_tab_1", dgp_out$distrConfig,
                     model_out$assumedDistrConfig,
                     model_out$MLEResult,
                     model_out$numXAssumed,
                     model_out$assumedXVals)

}

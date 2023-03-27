#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
runGov2k1 <- function(){
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      #onStart = onStart,
      # showcase mode doesn't currently work for files
      # in /R directory
      options = list("launch.browser" = "T"),
      enableBookmarking = NULL,
      uiPattern = "/",
    ),
    golem_opts = list()
  )
}

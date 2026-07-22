#' Run the Shiny Application
#'
#' @param host Interface on which to listen.
#' @param port TCP port, or `NULL` to let Shiny select one.
#' @param launch_browser Whether to open a local browser.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
runGov2k1 <- function(
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", NULL),
  launch_browser = interactive()
) {
  app <- with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(
        spinner.color = "#9a2b35",
        spinner.size = 0.7
      ),
      enableBookmarking = NULL,
      uiPattern = "/"
    ),
    golem_opts = list()
  )

  shiny::runApp(
    app,
    host = host,
    port = port,
    launch.browser = launch_browser
  )
}

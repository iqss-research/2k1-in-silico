#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = function(){
    # oldw <<- getOption("warn")
    pkgEnv$tutorialText <- read.csv(app_sys("tutorialText.csv"))
    # options(warn = -1)#, shiny.fullstacktrace = T)
    onStop(function(){
      # options(warn = oldw)

    })
  },
  options = list("launch.browser" = "T"),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

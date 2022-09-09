#' This function runs the Gov2k1inSilico Shiny Application.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
runGov2k1 <- function() {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = function(){
        pkgEnv$tutorialText <- read.csv(app_sys("tutorialText.csv"))
        oldw <<- getOption("warn")
        options(warn = -1)#, shiny.fullstacktrace = T)

        onStop(function(){
          options(warn = oldw)
        })
      },
      options = list("launch.browser" = "T"),
      enableBookmarking = NULL,
      uiPattern = "/"
    ),
    golem_opts = list()
  )
}

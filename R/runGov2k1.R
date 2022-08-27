#' This function runs the Gov2k1inSilico Shiny Application. It accepts most
#' Shiny options, with \code{launch.browser = TRUE} by default.
#'
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
runGov2k1 <- function(
  options = list("launch.browser" = "T")
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = function(){
        pkgEnv$tutorialText <- read.csv(app_sys("tutorialText.csv"))
      },
      options = options,
      enableBookmarking = NULL,
      uiPattern = "/"
    ),
    golem_opts = list()
  )
}

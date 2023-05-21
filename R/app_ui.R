#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import data.table
#' @import markdown
#' @import katex
#' @import dplyr
#' @import ggplot2
#' @import shinybrowser
#' @import shinycssloaders
#' @noRd
app_ui <- function(request) {
  options(warn=-1)
  shinybrowser::detect()
  tagList(

    # Adding external resources
    golem_add_external_resources(),

    # Application UI logic
    navbarPage(
      id = "tabs",
      position = "fixed-top",
      header = tags$head(
        # use Katex to autorender all tex: https://katex.org/docs/autorender.html
        tags$link(rel="stylesheet",
                  href="https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/katex.min.css",
                  integrity="sha384-vKruj+a13U8yHIkAyGgK1J3ArTLzrFGBbBc0tDp4ad/EyewESeXE/Iv67Aj8gKZ0",
                  crossorigin="anonymous"),
        # tags$script(defer="",
        #             src="https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/katex.min.js",
        #             integrity="sha384-PwRUT/YqbnEjkZO0zZxNqcxACrXe+j766U2amXcgMg5457rve2Y7I6ZJSm2A0mS4",
        #             crossorigin="anonymous"),
        # tags$script(defer="",
        #             src="https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/contrib/auto-render.min.js",
        #             integrity="sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05",
        #             crossorigin="anonymous",
        #             onload="renderMathInElement(document.body, {strict: false});"),
      ),
      title=div(
        img(
          src = "www/2k1-logo-icon.png",
          id = "shield",
          style = "cursor: pointer;"),
        shinyjs::useShinyjs(),
        tags$b("  in Silico"),
        class="titleDiv",
        id = "titleDiv",
        style = "cursor: pointer;"),
      windowTitle = " in Silico",
      theme = bslib::bs_theme(
        version = 3,
        bootswatch = "yeti",
        primary = iqOrangeStr,
        "navbar-default-bg" = iqOrangeStr,
      ),
      selected = "Introduction",
      ## INTRO TAB
      tabPanel(
        title = "Introduction",
        id = "Introduction",

        uiOutput("dgp_arrow"),
        #tags$div(class="glyphicon glyphicon-chevron-up bounce"),
        textOutput("browserwidth",inline=TRUE),
        includeMarkdown(app_sys("app/www/introduction.Rmd")),
        # actionButton(inputId = "gotodgp",
        #              label="Get Started",
        #              class="button-start"),
        tags$link(rel = "stylesheet",
                  type="text/css",
                  href="custom.css")
      ),

      mod_dgp_tab_ui("dgp_tab_1"),

      mod_model_tab_ui("model_tab_1"),

      mod_qoi_tab_ui("qoi_tab_1"),


      footer = tags$footer(
        tags$style(type="text/css", "body {padding-top: 70px;}"),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"))
  ))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "GOV 2001 in Silico"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}

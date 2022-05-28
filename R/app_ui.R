#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      id = "tabs",
      header = tags$head(
        shinyjs::useShinyjs(),
        withMathJax()
      ),
      title=div(
        img(
          src = "www/2k1-logo-icon.png",
          id = "shield",
          style = "cursor: pointer;"),
        tags$b("  in Silico"), class="titleDiv"),
      windowTitle = " in Silico",
      theme = bslib::bs_theme(
        version = 3,
        bootswatch = "yeti",
        primary = iqOrangeStr,
        "navbar-default-bg" = iqOrangeStr,
      ),
      selected = uiOutput("distrNameOutput"),
      tabPanel(
        title = "Introduction",
        id = "Introduction",
        uiOutput("introductoryText")
      ),
      tabPanel(
        title = uiOutput("distrNameOutput"),
        fluidRow(
          column(
            4,
            div(
              uiOutput("dgpChoiceUI"),
              class = "distrInput",
              # helperMaker("DGP Choice"),
            ),
          ),
        ),
        hr(),
        column(4, id = "sliders",
               fluidRow(
                 column(
                   12,
                   uiOutput("distrTex"),
                   # helperMaker("Probability Model")
                   ),
                 column(
                   12,uiOutput("obsSlider"),
                   # helperMaker("Observation Choice")
                   ),
                 column(
                   12,
                   uiOutput( #TODO: toggle divs with removeUI
                     "xChoiceDiv",
                     style = "padding-left:15px;"),
                   # helperMaker("Covariates")
                 ),
                 uiOutput("paramSlider")
               ),
               hr(),
               column(
                 12,
                 fluidRow(
                   uiOutput("dataHeader"),
                   div(
                     htmlOutput("outcomeDisplayP"),
                     style= "padding-top:15px;padding-left:15px",
                     width = "50px"),
                   # helperMaker("Randomly Generated Data"),
                 )
               ),
        ),
      )

    )
  )
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
      app_title = "testPackageGolem"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()

  )
}


# basic example
shinyApp(
  ui = fluidPage(
    selectInput("variable", "Variable:",
                c("Cylinders" = "cyl",
                  "Weight" = "wt",
                  "Transmission" = "am",
                  "Gears" = "gear",
                  "Horsepower" = "hp"), multiple = T),
    tableOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderTable({
      mtcars["mpg", input$variable, drop = FALSE]
    }, rownames = TRUE)
  }
)
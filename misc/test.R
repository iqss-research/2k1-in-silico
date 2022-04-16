library(shiny)
library(rintrojs)

ui <- shinyUI(fluidPage(
  introjsUI(),
  mainPanel(
    textInput("intro","Enter an introduction"),
    actionButton("btn","Press me")
  )
)
)

server <- shinyServer(function(input, output, session) {
  
  steps <- reactive(data.frame(element = c(NA,"#btn"),
                               intro = c(input$intro,"This is a button")))
  
  observeEvent(input$btn,{
    introjs(session,options = list(steps=steps()))
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
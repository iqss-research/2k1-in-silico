library(shiny)
library(shinyjs)
library(shinyBS)


ui <-shinyUI(fluidPage(useShinyjs(),
                       # press this button to trigger the popover
                       actionButton("addPopover", "Add Popover"),
                       
                       # a disabled button
                       disabled(actionButton("disabledButton", "This button is disabled")),
                       
                       # the popover to appear over the disabled button
                       bsPopover("disabledButton", "Popover", "Some text", trigger="manual"),
                       
                       # the script to trigger the popover
                       uiOutput("trigger")))


server <- shinyServer(function(input,output, session){
  
  # on checkbox selection, disable button and trigger the popover
  output$trigger <- renderUI({
    input$addPopover
    tags$script("$('#disabledButton').popover('toggle');")
  })
})

shinyApp(ui,server)

ui <- navbarPage("App Title",
           tabPanel("Plot"),
           navbarMenu("More",
                      tabPanel("Summary"),
                      "----",
                      "Section header",
                      tabPanel("Table")
           )
)

server <- function(input, output, session) {}

# Run the application 
shinyApp(ui = ui, server = server)

library("shiny")
library("ggplot2")

ui <- pageWithSidebar(
  headerPanel("Tooltips in ggplot2 + shiny"),
  
  sidebarPanel(
    HTML("Tooltips are managed by combination of shiny+ggplot hover functionality",
         "and css styles. By setting hover argument of 'plotOutput' we could access",
         "hover data from the server side, as an ordinary input. Hover input is",
         "a list with: position of cursor ON the image; domain - that is",
         "values of variables at the plotting area edges; range - that is position",
         "of plotting area edges in pixels relative to whole image element.",
         "Additionally for ggplot used mappings are returned. </br>",
         "To create tooltip first we need to identify position of the cursor",
         "inside the image element. We do it by calculating distances from left and",
         "top edge of image element from hover data. Then we create tooltip, in this",
         "app it is 'wellPanel' with some info inside, and set 'position' property",
         "to 'absolute' and set 'left' and 'top' properties to calculated values.",
         "However, 'absolute' position is defined as relative to the nearest positioned",
         "ancestor. Because we want to position tooltip inside the image, we need",
         "to put both 'plotOutput' with image and 'uiOutput' with tooltip content",
         "inside additional 'div' element with 'position' property set to 'relative'.",
         "We don't set top, left etc. for this element, so the actual position of",
         "the image doesn't change - it's edges are identical as previously, so",
         "we can use 'div' (for positioning tooltip) as substitute for image. </br>"),
    width = 3
  ),
  
  mainPanel(
    
    # this is an extra div used ONLY to create positioned ancestor for tooltip
    # we don't change its position
    div(
      style = "position:relative",
      plotOutput("scatterplot", 
                 hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
      uiOutput("hover_info")
    ),
    width = 7
  )
)

server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    ggplot(mtcars, aes(x = mpg, y = hp)) +
      geom_point()
  })
  
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(mtcars, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Car: </b>", rownames(point), "<br/>",
                    "<b> mpg: </b>", point$mpg, "<br/>",
                    "<b> hp: </b>", point$hp, "<br/>",
                    "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px)))
    )
  })
}

runApp(list(ui = ui, server = server))


server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  
  
  observeEvent(input$button1, {
    output$graf <- renderPlot({
   
      net <- narisi("", input$dir, input$st_ogl, input$Dim_x, input$Dim_y)$network
      
      plot.igraph(net)
  print(input$dir, input$st_ogl, input$Dim_x)
   
    })
  })
  
  
  
}
  
  
  
  
  




# tu je server funkcija

server <- function(input, output) {
  
  
  ### ZAVIHEK VNOS GRAFA ###
  
  # uporabnika vprasamo ali ima graf ze izbran ali naj mu ga predlaga program
  observeEvent( input$generate, {
    
    if (input$generate == 1){
      output$text_2 <- renderText({ "Super, podati mi moras zeljeno stevilo vozlisc in povezav. Lahko si izbereš tudi poln graf." })
    } else if (input$generate == 2){
      output$text_2 <- renderText({ "Torej moras vnesti se povezavno matriko svojega grafa." })
    } else {
      output$text_2 <- renderText({ "Prosim, izpolni zgornje polje!" })
    }
    
  })
  
  observeEvent( input$dir, {
    
    if (input$dir == 2){
      output$text_1 <- renderText({ "Graf bo usmerjen!" })
    } else if (input$dir == 1){
      output$text_1 <- renderText({ "Graf bo neusmerjen!" }) 
    } else {
      output$text_1 <- renderText({ "Prosim, izpolni zgornje polje!" }) 
    }
  })
  
  observeEvent( input$button2, {
    if (input$vozl_2 > 0 & input$generate == 2){
      
      output$text_3 <- renderText({
        "Sedaj lahko vneses elemente povezavne matrike"})
      
      output$vnos <- renderUI({
        lapply((1:input$vozl_2), function(i) {
          textInput(paste0("v",i), paste0("v",i), "0,1,0")})
      })
      
    } else {
      output$text_3 <- renderText({
        "Prosim, ponovno preveri vnesene atribute!"})
      
    }
  })
  
  
  
  df_react <- reactiveValues(graf=NULL)
  
  # nato mu narocimo, naj narise graf
  observeEvent( input$button1, {
    
    if (input$generate == 1){
      if (input$polnost == 1){
        output$graf <- renderPlot({
          
          #df_react$graf = narisi_poln(input$vozl_1, input$dir)
          #plot.igraph(df_react$graf$network)
          
          plot.igraph(narisi_poln(input$vozl_1, input$dir))
        })
      } else if (input$polnost == 2){
        output$graf <- renderPlot({
          
          #df_react$graf = narisi_izbor(input$vozl_1, input$povez, input$dir)
          #plot.igraph(df_react$graf$network)
          
          plot.igraph(narisi_izbor(input$vozl_1, input$povez, input$dir)$network)
        })
      }
      
    } else if (input$generate == 2){
      
      matrika <- matrix(c(as.numeric(sapply(1:input$vozl_2, function(i) 
        unlist(strsplit(input[[paste0("v",i)]], ","))))), input$vozl_2, input$vozl_2, byrow=TRUE)
      
      output$graf <- renderPlot({
        
        #df_react$graf = narisi_pripravljen(matrika, input$dir)
        #plot.igraph(df_react$graf$network)
        
        plot.igraph(narisi_izbor(input$vozl_1, input$povez, input$dir)$network)
        
      })
    }
  })
  
  
  
  
  
  
  
  }

shinyApp(ui=ui, server=server) 
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
          
          df_react$graf = narisi_poln(input$vozl_1, input$dir)
          #plot.igraph(df_react$graf)
          
          plot.igraph(df_react$graf)
        })
      } else if (input$polnost == 2){
        output$graf <- renderPlot({
          
          df_react$graf = narisi_izbor(input$vozl_2, input$povez, input$dir)
          #plot.igraph(df_react$graf$network)
          
          plot.igraph(df_react$graf$network)
        })
      }
      
    } else if (input$generate == 2){
      
      matrika <- matrix(c(as.numeric(sapply(1:input$vozl_3, function(i) 
        unlist(strsplit(input[[paste0("v",i)]], ","))))), input$vozl_3, input$vozl_3, byrow=TRUE)
      
      output$graf <- renderPlot({
        
        df_react$graf = narisi_pripravljen(matrika, input$dir)
        #plot.igraph(df_react$graf$network)
        
        plot.igraph(df_react$graf$network)
        
      })
    }
  })
  
  
  
  
  ### ZAVIHEK LASTNOSTI ###
  
  observeEvent( input$button3, {
    
    req(df_react$graf)
    
    
    if (input$characteristic == 1){
      output$poizvedba <- renderText({
        seznam_stopenj <- stopnje(df_react$graf)
        #paste(seznam_stopenj)
        paste(names(seznam_stopenj), seznam_stopenj)
      })
      output$text_4 <- renderText({
        "Tukaj je seznam stopenj tvojega grafa:"
      })
      
    } else if (input$characteristic == 2){
      output$poizvedba <- renderUI({
        seznam_ciklov <- najdi_cikle(df_react$graf)
        vsi <- list()
        for (cikel in (1:length(seznam_ciklov))){
          vsi[cikel] <- seznam_ciklov[[cikel]]
        }
        print(vsi)
      })
      
    } else if (input$characteristic == 3){
      stevilo_povezav <- povezave(df_react$graf)
      output$text_4 <- renderText({
        paste("Tvoj graf ima", stevilo_povezav, "povezav.")
      })
      
    } else if (input$characteristic == 5){
      
      diam <- najdaljsa_razdalja(df_react$graf)
      print(diam)
      output$text_4 <- renderText({
        paste("Najdaljsa razdalja v grafu meri", diam ,"enot.") 
      })
      
    } else if (input$characteristic == 6){
      
      radij <- najkrajsa_razdalja(df_react$graf)
      print(radij)
      output$text_4 <- renderText({
        paste("Najkrajsa razdalja v grafu meri", radij ,"enot.") 
      })
      
    } else if (input$characteristic == 4){
      bipartite <- dvodelen(df_react$graf)
      
      if (bipartite$dvo == TRUE){
        output$text_4 <- renderText({
          "Tvoj graf je dvodelen, tu sta mozni komponenti"
        })
        output$poizvedba <- renderPlot({
          plot(bipartite$plot_1)
          plot(bipartite$plot_2)
        })
      } else {
        
        output$text_4 <- renderText({
          "Tvoj graf ni dvodelen!"
          
        })
        
      }
      
      
    } else if (input$characteristic == 7){
      
      komp <- komponente(df_react$graf)
      output$text_4 <- renderText({
        paste(komp)
        
      })
      
      
      
      
      
    }
    
    
  })
  
  
  
  
  }

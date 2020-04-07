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
      
      #matrixInput("utezi", matrix(0,4,5), class="numeric")
      
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
          
          plot.igraph(df_react$graf$network)
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
        seznam_stopenj <- stopnje(df_react$graf$network)
        #paste(seznam_stopenj)
        paste(names(seznam_stopenj), seznam_stopenj)
      })
      output$text_4 <- renderText({
        "Tukaj je seznam stopenj tvojega grafa:"
      })
      
    } else if (input$characteristic == 2){
      if (is_directed(df_react$graf)=="TRUE" & DAG(df_react$graf) == "TRUE"){
        output$text_4 <- renderText({
          "Tvoj graf je aciklicen usmerjen graf!"
        })
      }

      output$poizvedba <- renderUI({
        seznam_ciklov <- najdi_cikle(df_react$graf$network)
        vsi <- list()
        for (cikel in (1:length(seznam_ciklov))){
          vsi[cikel] <- seznam_ciklov[[cikel]]
        }
        print(vsi)
      })
      
    } else if (input$characteristic == 3){
      stevilo_povezav <- povezave(df_react$graf$network)
      output$text_4 <- renderText({
        paste("Tvoj graf ima", stevilo_povezav, "povezav.")
      })
      output$poizvedba <- NULL
      
    } else if (input$characteristic == 5){
      
      diam <- najdaljsa_razdalja(df_react$graf$network)
      print(diam)
      output$text_4 <- renderText({
        paste("Najdaljsa razdalja v grafu meri", diam ,"enot.") 
      })
      output$poizvedba <- NULL
      
    } else if (input$characteristic == 6){
      
      radij <- najkrajsa_razdalja(df_react$graf$network)
      print(radij)
      output$text_4 <- renderText({
        paste("Najkrajsa razdalja v grafu meri", radij ,"enot.") 
      })
      output$poizvedba <- NULL
      
    } else if (input$characteristic == 4){
      bipartite <- dvodelen(df_react$graf$network)
      
      if (bipartite == TRUE){
        output$text_4 <- renderText({
          "Tvoj graf je dvodelen!"
        })
        output$poizvedba <- NULL
        
      } else {
        output$text_4 <- renderText({
          "Tvoj graf ni dvodelen!"
        })
        output$poizvedba <- NULL
        
      }
      
      
    } else if (input$characteristic == 7){
      
      komp <- komponente(df_react$graf$network)
      output$text_4 <- renderText({
        paste(komp)
        
      })
      
      
      
      
      
    }
    
    
  })
  
  
  
  
  ### ZAVIHEK PROBLEMI ###
  
  
  output$dimenzija <- renderUI({
    req(df_react$graf)
    length(E(df_react$graf$network)) 
  })
     
 
  
  observeEvent(input$problem,{
    req(df_react$graf)
    if (input$problem == 1){
      output$text_5 <- renderText({
        "Utezi doloci po vrsti, spodaj prikazanim povezavam iz vozlisc v1 v v2."
      })
      
      #output$utezi <- renderUI({
      #  lapply((1:input$vozl_2), function(i) {
      #    textInput(paste0("u",i), paste0("u",i), "1,2,3")})
      #})
      
      output$edges <- renderGvis({
      data <- as.data.frame(get.edgelist(df_react$graf$network, names=TRUE))
       gvisTable(data)
       })
      
    }
    
    
    
   })
    
    
    
    
    observeEvent(input$button4, {
      #vzamemo graf, ki je trenutno v uporabi
      req(df_react$graf)

      if (input$problem == 1){
      #preberemo vnesene utezi uporabnika
      vect_utezi <- as.numeric(unlist(strsplit(input$utezi, ",")))
      
      #### sestavimo matriko utezi
      matrika_utezi <- sestavi_matriko_utezi(df_react$graf$network, vect_utezi)
      #### matriko uporabimo kot cene v TSP
      
      if (length(vect_utezi) != length(E(df_react$graf$network))){
        output$text_6 <- renderText({
          "Ponovno preveri vnos utezi. Vsaki povezavi grafa pripada tocno ena vrednost."
        })
      } else {
        output$text_6 <- renderText({
          "Super, vsaki povezavi grafa sedaj pripada tocno ena vrednost."
        })
        }
      
      # postavimo omejitev, da graf sploh vsebuje zacetno vozlisce in da ima vsaj 2 originalna soseda, sicer bo obhod zelo drag
      if (input$start > length(E(df_react$graf$network)) | length(neighbors(df_react$graf$network, input$start))<2){
        output$text_7 <- renderText({
          "Ponovno preveri ustreznost zacetnega vozlisca. Ce ta nima vsaj dveh sosedov v tvojem grafu, bo obhod zelo drag."
        })
      } else {
        output$text_7 <- renderText({
          paste("Pocakaj, resil ti bom problem TSP.")
        })
      }
        
      output$text_8 <- renderText({
        "To je cena najcenejse poti trgovskega potnika:"
      })
      
      dolzina <- PTP(df_react$graf$network, input$start, matrika_utezi)$dolzina
      
      output$TSP <- renderUI({
        PTP(df_react$graf$network, input$start, matrika_utezi)$dolzina
      }) 
      
      output$pot <- renderGvis({
        data <- as.data.frame(PTP(df_react$graf$network, input$start, matrika_utezi)$pot)
        gvisTable(data)
      })
      
      if (dolzina > 10000){
        output$text_9 <- renderText({
            "Najcenejsega obhoda na tvojem grafu ni!"
        })
        } else {
        output$text_9 <- NULL
        }
      
      
      
      
        

   }
 })
    
}    
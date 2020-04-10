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
    if (input$vozl_3 > 0 & input$generate == 2){
      
      output$text_3 <- renderText({
        "Sedaj lahko po vrsticah vneses elemente povezavne matrike."
        })
      
      output$vnos <- renderUI({
        lapply((1:input$vozl_3), function(i) {
          textInput(paste0("v",i), paste0("v",i), paste("0,0,1,... vnesi",input$vozl_3,"cifer"))})
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
          plot.igraph(df_react$graf$network)
        })
      } else if (input$polnost == 2){
        output$graf <- renderPlot({
          
          df_react$graf = narisi_izbor(input$vozl_2, input$povez, input$dir)
          plot.igraph(df_react$graf$network)
        })
      }
      
    } else if (input$generate == 2){
      
      matrika <- matrix(c(as.numeric(sapply(1:input$vozl_3, function(i) 
        unlist(strsplit(input[[paste0("v",i)]], ","))))), input$vozl_3, input$vozl_3, byrow=TRUE)
      
      output$graf <- renderPlot({
        
        df_react$graf = narisi_pripravljen(matrika, input$dir)
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
      output$ciklicnost <- renderText({
        cikli <- poisci_cikle(df_react$graf$network)
        paste(cikli)
          })
      
      if (is_directed(df_react$graf$network)=="TRUE" & DAG(df_react$graf$network) == "TRUE"){
        output$text_4 <- renderText({
          "Tvoj graf je acikličen in usmerjen!"
           })
      } else {
        output$text_4 <- renderText({
          "Tu je seznam ciklov grafa:"
        })
        }
      
      } else if (input$characteristic == 3){
      stevilo_povezav <- povezave(df_react$graf$network)
      output$text_4 <- renderText({
        paste("Tvoj graf ima", stevilo_povezav, "povezav.")
      })
      output$poizvedba <- NULL
      
    } else if (input$characteristic == 5){
      
      diam <- najdaljsa_razdalja(df_react$graf$network)
      output$text_4 <- renderText({
        paste("Najdaljsa razdalja v grafu meri", diam ,"enot.") 
      })
      output$poizvedba <- NULL
      
    } else if (input$characteristic == 6){
      
      radij <- najkrajsa_razdalja(df_react$graf$network)
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
  
  observeEvent(input$problem,{
    req(df_react$graf)
    
    
    if (input$problem == 1){
      #resujemo problem trgovskega potnika
      output$text_5 <- renderText({
        "Utezi doloci po vrsti, spodaj prikazanim povezavam iz vozlisc v1 v v2."
      })
      output$edges_1 <- renderGvis({
      data <- as.data.frame(get.edgelist(df_react$graf$network, names=TRUE))
       gvisTable(data)
       })
      
      
    } else if (input$problem == 4){
      output$edges_2 <- renderGvis({
        data <- as.data.frame(get.edgelist(df_react$graf$network, names=TRUE))
        gvisTable(data)
      })
    
      
    } else if (input$problem == 5){
      output$edges_3 <- renderGvis({
        data <- as.data.frame(get.edgelist(df_react$graf$network, names=TRUE))
        gvisTable(data)
      })
    }
  
   })
  
  
  
  
  observeEvent(input$button4, {
      #vzamemo graf, ki je trenutno v uporabi
      req(df_react$graf)

    
    
    
      if (input$problem == 1){
      # resujemo problem trgovskega potnika
        
      #preberemo vnesene utezi uporabnika
      vect_utezi <- as.numeric(unlist(strsplit(input$utezi_1, ",")))
      ### sestavimo matriko utezi
      matrika_utezi <- sestavi_matriko_utezi(df_react$graf$network, vect_utezi)
      ### matriko uporabimo kot cene v TSP
      
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
      output$TSP <- renderText({
        PTP(df_react$graf$network, input$start, matrika_utezi)$dolzina
      }) 
      output$pot <- renderText({
        labels(PTP(df_react$graf$network, input$start, matrika_utezi)$pot)
      })
      
      # razlozimo kaj pomeni drag obhod
      if (dolzina > 10000){
        output$text_9 <- renderText({
            "Graf ima zelo malo povezav, zato najcenejsega obhoda na tvojem grafu ni!"
        })
        } else {
        output$text_9 <- NULL
        }
      
      
      
      
        

      } else if (input$problem == 2){
    # resujemo problem barvanja grafa
     
        output$krom_num <- renderText({
          paste("Tvoj graf lahko obarvamo z najmanj", kromaticno_stevilo(df_react$graf$network)$krom_stevilo, "barvami. Resitev vidis spodaj!")
        })
        output$barvanje <- renderPlot(
          plot(kromaticno_stevilo(df_react$graf$network)$slika)
          )

        
        
        
        
      } else if (input$problem == 3){
        # resujemo problem ravninskosti grafa
        
        if (ravninski(df_react$graf$network)$planar == TRUE){
          output$planarity <- renderText({
            "Tvoj graf je ravninski. Prepricaj se iz spodnje slike."
          })
        } else if (ravninski(df_react$graf$network)$planar == FALSE){
          output$planarity <- renderText({
            "Tvoj graf ni ravninski. Prepricaj se iz spodnje slike."
          })
        }
        output$ravnina <- renderPlot(
          plot(df_react$graf$network, layout = layout.fruchterman.reingold)
        )
        
        
        
        
        
        
      } else if (input$problem == 4){
        # resujemo problem iskanja najcenejse poti med dvema vozliscema
        
        vect_utezi <- as.numeric(unlist(strsplit(input$utezi_2, ",")))
        output$najceneje <- renderText({
          dolzina <- najdi_najkrajso_pot(df_react$graf$network, input$zacni, input$finish, vect_utezi)$dolzina
          paste("Najcenejsa pot stane", dolzina, "enot.")
        })
        output$poti <- renderText({
          vse_poti <- najdi_najkrajso_pot(df_react$graf$network, input$start, input$finish, vect_utezi)$poti
        })
        
        
        
        
        
        
        
      } else if (input$problem == 5){
        # resujemo problem minimalne elektricne napeljave
        
        vect_utezi <- as.numeric(unlist(strsplit(input$utezi_3, ",")))
        output$min_cena <- renderText({
          cena <- PMEN(df_react$graf$network, vect_utezi)$cena
          paste("Najcenejsa elektricna napeljava stane", cena, "enot.")
        })
        output$napeljava <- renderPlot(
          plot(PMEN(df_react$graf$network, vect_utezi)$napeljava)
        )
        
        
        
        
        
        
        
      } else if (input$problem == 6){
        # resujemo problem ugasanja luci
        
        
        
        
        
        
      }
    
    
    
    
    
    
    
    
    
    
    
    
    
 })
    
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}    


















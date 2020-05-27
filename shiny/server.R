# tu je server funkcija, ki se kliče v datoteki zagon.R

server <- function(input, output) {
  
  
  current_graph <- reactive({
    req(df_react$graf)
    
    input$uporabi_line
    input$uporabi_kompl
    
    if (input$uporabi_line == 1){
      df_react$graf <- output$povezavni
    } 
    if (input$uporabi_kompl == 1){
      df_react$graf <- output$komplementaren
    }
    
    df_react$graf
    
  })  
  
  
  ### ZAVIHEK VNOS GRAFA ###
  
  # uporabnika vprasamo ali ima graf ze izbran ali naj mu ga predlaga program
  observeEvent( input$generate, {
    
    if (input$generate == 1){
      output$text_2 <- renderText({ "Super, podati mi moraš željeno število vozlišč in povezav. Lahko si izbereš tudi poln graf." })
    } else if (input$generate == 2){
      output$text_2 <- renderText({ "Torej moraš vnesti že povezavno matriko svojega grafa." })
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
        "Sedaj lahko po vrsticah vneseš elemente povezavne matrike."
        })
      
      output$vnos <- renderUI({
        lapply((1:input$vozl_3), function(i) {
          textInput(paste0("v",i), paste0("v",i), paste("0,0,1,... vnesi",input$vozl_3,"ničel ali enic"))})
      })
      
    } else {
      output$text_3 <- renderText({
        "Prosim, ponovno preveri vnešene atribute!"})
      
    }
  })
  
  df_react <- reactiveValues(graf=NULL, nov_graf=NULL, ok=FALSE)
  
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
    #df_react$graf <- current_graph()

    
    if (input$characteristic == 1){
      output$poizvedba_st <- renderText({
        seznam_stopenj <- stopnje(df_react$graf$network)
        paste(names(seznam_stopenj), seznam_stopenj)
      })
      output$text_4 <- renderText({
        "Tukaj je seznam stopenj tvojega grafa:"
      })
      
      
      
    } else if (input$characteristic == 2){
      output$ciklicnost <- renderText({
        cikli <- precisti_cikle(isci_cikle(df_react$graf$network))
        if (length(cikli) == 0){
          paste("Seznam je prazen.. Tvoj graf nima nobenega cikla.")
        } else {
        paste(names(cikli), cikli)
         }
        })
      
      if (is_directed(df_react$graf$network)=="TRUE" & DAG(df_react$graf$network) == "TRUE"){
        output$text_5 <- renderText({
          "Tvoj graf je acikličen in usmerjen!"
           })
      } else {
        output$text_5 <- renderText({
          "Tu je seznam ciklov grafa:"
        })
        }
      
      
      
      
      } else if (input$characteristic == 3){
      stevilo_povezav <- povezave(df_react$graf$network)
      output$text_6 <- renderText({
        paste("Tvoj graf ima", stevilo_povezav, "povezav.")
      })
      
      
      
      
      }  else if (input$characteristic == 4){
        bipartite <- dvodelen(df_react$graf$network)$dvo
        postavitev <- dvodelen(df_react$graf$network)$logical
        
        if (bipartite == TRUE){
          output$text_7 <- renderText({
            "Tvoj graf je dvodelen! Prepričaj se s spodnjim prikazom."
          })
          output$poizvedba_dvo <- renderPlot({
            graf <- df_react$graf$network
            plot(graf, layout=layout_as_bipartite(graf, types=postavitev))
          })
          
        } else {
          output$text_7 <- renderText({
            "Tvoj graf ni dvodelen!"
          })
          output$poizvedba_dvo <- NULL
        }
    
    
   }  else if (input$characteristic == 5){
      
      diam <- najdaljsa_razdalja(df_react$graf$network)
      output$text_8 <- renderText({
        paste("Diameter grafa meri", diam ,"enot.") 
      })
      
      
      
      
    } else if (input$characteristic == 6){
      
      radij <- najkrajsa_razdalja(df_react$graf$network)
      output$text_9 <- renderText({
        paste("Radij grafa meri", radij ,"enot.") 
      })
      
      
      
      
    } else if (input$characteristic == 7){
      
      komp <- komponente(df_react$graf$network)$a
      graf <- komponente(df_react$graf$network)$graf
      output$text_10 <- renderText({
        paste(komp)
      })
      output$poizvedba_komp <- renderPlot({
        plot(graf)
      })
      
      

    } else if (input$characteristic == 8){
      # resujemo problem iskanja najcenejse poti med dvema vozliscema
      
      vect_utezi <- as.numeric(unlist(strsplit(input$utezi_2, ",")))
      output$najceneje <- renderText({
        dolzina <- najdi_min_pot(df_react$graf$network, input$zacni, input$finish, vect_utezi)$min_dolzina
        paste("Najcenejša pot stane", dolzina, "enot.")
      })
      output$minimum_poti <- renderPlot({
        primer_poti <- najdi_min_pot(df_react$graf$network,input$zacni,input$finish, vect_utezi)$min_primeri
        print(primer_poti)
        graf <- set_vertex_attr(df_react$graf$network, name="color", index = primer_poti, value="red")
        plot(graf)
      })
      
      
      
      
    } else if (input$characteristic == 9){
      # resujemo problem minimalne elektricne napeljave
      
      vect_utezi <- as.numeric(unlist(strsplit(input$utezi_3, ",")))
      output$min_cena <- renderText({
        cena <- PMEN(df_react$graf$network, vect_utezi)$cena
        paste("Postavitev minimalnega vpetega drevesa stane", cena, "enot.")
      })
      output$napeljava <- renderPlot(
        plot(PMEN(df_react$graf$network, vect_utezi)$napeljava)
      )
      
      
      
      
      
    }  else if (input$characteristic == 10){
      # resujemo problem barvanja grafa
      output$krom_num <- renderText({
        paste("Tvoj graf lahko obarvamo z najmanj", kromaticno_stevilo(df_react$graf$network)$krom_stevilo, "barvami. Re?itev vidi? spodaj!")
      })
      output$barvanje <- renderPlot(
        plot(kromaticno_stevilo(df_react$graf$network)$slika)
      )
      
      
      
      
      
    } else if (input$characteristic == 11){
      # resujemo problem ravninskosti grafa
      if (ravninski(df_react$graf$network)$planar == TRUE){
        output$planarity <- renderText({
          "Tvoj graf je ravninski. Prepričaj se iz spodnje slike."
        })
      } else if (ravninski(df_react$graf$network)$planar == FALSE){
        output$planarity <- renderText({
          "Tvoj graf ni ravninski. Prepričaj se iz spodnje slike."
        })
      }
      output$ravnina <- renderPlot(
        plot(df_react$graf$network, layout = layout.fruchterman.reingold)
      )

      
      
      
    } else if (input$characteristic == 12){
      # resujemo problem ugotavljanja graficnosti zaporedja
      if (input$graficnost ==1){
        vhodne <- as.numeric(unlist(strsplit(input$in_stopnje, ",")))
        izhodne <- as.numeric(unlist(strsplit(input$out_stopnje, ",")))
        rezultat <- graficnost(izhodne, vhodne)
        if (length(izhodne) != length(vhodne)){
          output$preveri_stopnje <- renderText({
            "Ponovno preveri vnos stopenj. ?tevilo vhodnih mora biti enako ?tevilu izhodnih!"
          })
        } else {
          output$preveri_stopnje <- renderText({
            "Super, stopnje si vnesel pravilno!"
          })
        }
        
      } else if (input$graficnost ==2){
        stopnje <- as.numeric(unlist(strsplit(input$stopnje, ",")))
        rezultat <- graficnost(stopnje, NULL)
      }
      if (rezultat$obstaja == TRUE){
        output$text_11 <- renderText({
          "Iz danih stopenj vozlišč bi lahko sestavil graf. Tu je primer:"
        })
        output$graf_iz_zaporedja <- renderPlot(
          plot(rezultat$primer)
        )
      } else {
        output$text_11 <- renderText({      
          "Graf z danimi stopnjami vozlišč ne obstaja."
        })
        output$graf_iz_zaporedja <- NULL
      }
      
      
      
      
      
    }  else if (input$characteristic == 13){
      # resujemo problem iskanja eulerjevega cikla, sprehoda ali stevila povezav za risanje
      shinyjs::disable("button3")
      df_react$ok <- FALSE
        cikel <- Euler(df_react$graf$network)$cikel
        komentar_cikel <-  Euler(df_react$graf$network)$komentar_cikel
        poteze <- Euler(df_react$graf$network)$min_st_potez
        komentar <- paste("Minimalno število potez, s katerimi boš narisal svoj graf je", poteze, "tu pa je iskani Eulerjev cikel:")
        df_react$ok <- TRUE
      
      output$komentiraj <-renderText({
        if (df_react$ok == TRUE) {
          shinyjs::enable("button3")
          paste(komentar_cikel)
          paste(komentar)
        }
      })
      output$rezultat <-renderText({
        if (df_react$ok == TRUE) {
          shinyjs::enable("button3")
          paste(cikel)
        }
      })
      
      
      
      
      
    } else if (input$characteristic == 14){
      # resujemo problem iskanja povezavnega grafa
      output$povezavni <- renderPlot(
        plot(povezavni(df_react$graf$network)$network)
      )
      if (input$uporabi_line == 1){
        df_react$graf <- povezavni(df_react$graf$network)
      } else {
        df_react$graf <- df_react$graf
      }
      
      
      
    } else if (input$characteristic == 15){
      # resujemo problem iskanja komplementarnega grafa
      output$komplementaren <- renderPlot(
        plot(komplement(df_react$graf$network)$network)
      )
      if (input$uporabi_kompl == 1){
        df_react$graf <- komplement(df_react$graf$network)
      } else {
        df_react$graf <- df_react$graf
      }
    }
    
    
    
  })
  
  
  observeEvent(input$button3,{
     req(df_react$graf)
     #df_react$graf <- current_graph()
     
     output$graf_v_uporabi <- renderPlot(
       plot(df_react$graf$network)
               )
 
               })
  
  
  observeEvent(input$characteristic,{
    # vzamemo graf, ki je trenutno v uporabi
    req(df_react$graf)
    #df_react$graf <- current_graph()
 
    if (input$characteristic == 8){
      output$povezave_2 <- renderGvis({
        data <- as.data.frame(get.edgelist(df_react$graf$network, names=TRUE))
        gvisTable(data)
      })

    } else if (input$characteristic == 9){
      output$edges_3 <- renderGvis({
        data <- as.data.frame(get.edgelist(df_react$graf$network, names=TRUE))
        gvisTable(data)
      })

    }
  
  
  })
  
  
  
  
  ### ZAVIHEK PROBLEM TRGOVSKEGA POTNIKA ###
  
  observeEvent(input$button5,{
    # vzamemo graf, ki je trenutno v uporabi
    req(df_react$graf)
    #df_react$graf <- current_graph()
    #print(df_react$graf$network)
       
      #resujemo problem trgovskega potnika
      
      output$text_12 <- renderText({
        "Uteži določi po vrsti, spodaj prikazanim povezavam iz vozlišč v1 v v2."
      })
      output$povezave_1 <- renderGvis({
         data <- as.data.frame(get.edgelist(df_react$graf$network, names=TRUE))
         gvisTable(data)
       })
      
    
    })
  
  observeEvent(input$button4, {
      #vzamemo graf, ki je trenutno v uporabi
    req(df_react$graf)
    
      # resujemo problem trgovskega potnika
        shinyjs::disable("button4")
        
      #preberemo vnesene utezi uporabnika
      vect_utezi <- as.numeric(unlist(strsplit(input$utezi_1, ",")))
      ### sestavimo matriko utezi
      matrika_utezi <- sestavi_matriko_utezi(df_react$graf$network, vect_utezi)
      ### matriko uporabimo kot cene v TSP
      
      if (length(vect_utezi) != length(E(df_react$graf$network))){
        output$text_13 <- renderText({
          "Ponovno preveri vnos uteži. Vsaki povezavi grafa naj pripada točno ena vrednost."
        })
      } else {
        output$text_13 <- renderText({
          "Super, vsaki povezavi grafa sedaj pripada točno ena vrednost."
        })
      }
      
      # postavimo omejitev, da graf sploh vsebuje zacetno vozlisce in da ima vsaj 2 originalna soseda, sicer bo obhod zelo drag
      if (input$start > length(E(df_react$graf$network)) | length(neighbors(df_react$graf$network, input$start))<2){
        output$text_14 <- renderText({
          "Ponovno preveri ustreznost začetnega vozlišča. Če ta nima vsaj dveh sosedov v tvojem grafu, bo obhod zelo drag."
        })
      } else {
        output$text_14 <- renderText({
          paste("Počakaj, rešil ti bom problem TSP.")
        })
      }
      output$text_15 <- renderText({
        "To je cena najcenejše poti trgovskega potnika:"
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
        output$text_16 <- renderText({
            "Graf ima zelo malo povezav, zato najcenejšega obhoda na tvojem grafu ni! Vseeno ti ponujam obhod, ki bi ga uporabil, če bi manjkajoče povezave obstajale.."
        })
        } else {
        output$text_16 <- NULL
        }

      shinyjs::enable("button4")
      } 
    
          )

}    


















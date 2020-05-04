# tu je ui funkcija

body <- dashboardBody(
  tabItems(
    
    
    ### ZAVIHEK VNOS
    
    tabItem(tabName = "vnos",
            
            fluidRow(sidebarPanel(
              h3("Dobrodosel v svetu grafov!"),
              box(width=12, title ="VNOS ATRIBUTOV", status = "primary",
              selectInput("generate", "Naj ti graf generiram jaz, ali imas ze izbranega? ",
                          choices = list("generiraj" = 1,
                                         "imam_svoj_graf"=2,
                                         "/"=3), selected = 3),
              textOutput("text_2"),
              conditionalPanel(
                condition = "input.generate == '1'",
                selectInput(inputId = "polnost", "Naj bo graf poln?",
                            choices = list("DA" = 1, "NE" = 2, "/"=3), selected = 3),
                conditionalPanel(condition = "input.polnost == '1'",
                                 numericInput(inputId = "vozl_1",
                                              label = "Izberi stevilo vozlisc grafa",
                                              value = 0 )),
                conditionalPanel(condition = "input.polnost == '2'",
                                 numericInput(inputId = "vozl_2",
                                              label = "Izberi stevilo vozlisc grafa",
                                              value = 0 ),
                                 numericInput(inputId = "povez",
                                              label = "Izberi stevilo povezav grafa",
                                              value = 0 ))
              ),
              
              conditionalPanel(
                condition = "input.generate == '2'",
                numericInput(inputId = "vozl_3",
                             label = "Izberi stevilo vozlisc grafa",
                             value = 0 ),
                actionButton("button2", "Vnesi"),
                textOutput("text_3"),
                uiOutput("vnos")
                
              ),
              
              selectInput("dir", "Izberi se usmerjenost grafa",
                          choices = list("Graf ni usmerjen" = 1,
                                         "Graf je usmerjen" = 2,
                                         "nevem" = 3), selected = 3),
              textOutput("text_1"),
              actionButton("button1", "Narisi")
            )),
            
            box( 
              title = "PRIKAZ GRAFA", status = "success",
              plotOutput("graf"))
            )),
    
    
    
    ### ZAVIHEK LASTNOSTI
    
    tabItem(tabName = "lastnosti",
            
            fluidRow(sidebarPanel(
              h3("OSNOVNE GRAFOVSKE LASTNOSTI"),
              #p("Klikni gumb 'Posodobi', da najprej osvezis podatke o grafu, ki ga preiskujes :)"),
              box(width=12, title = "IZBOR LASTNOSTI", status = "primary",
              selectInput("characteristic", "Izberi iskano lastnost",
                           choices = list("stopnje" = 1, 
                                          "vsebovanost ciklov" = 2,
                                          "stevilo povezav" = 3,
                                          "dvodelnost" = 4,
                                          "premer/diameter" = 5,
                                          "polmer/radij" = 6,
                                          "povezanost" = 7,
                                          "iskanje najcenejse poti" = 8,
                                          "iskanje minimalnega vpetega drevesa"=9,
                                          "barvanje" = 10,
                                          "ravninskost" = 11,
                                          "graficnost" = 12,
                                          "Eulerjeve lastnosti" = 13,
                                          "povezavni graf"= 14,
                                          "komplementarni graf" = 15), selected =1),
              
              
              conditionalPanel(
                condition = "input.characteristic =='8'",
                p("Lotil si se iskanja najcenejse poti med dvema vozliscema. Najprej mi moras povedati kateri dve vozlisci naj gledam."),
                numericInput(inputId = "zacni", "Vnesi stevilko vozlisca, ki naj bo zacetek poti.", value = 0, min=0),
                numericInput(inputId = "finish", "Vnesi stevilko vozlisca, ki naj bo konec poti.", value = 0, min=0),
                p("Sedaj pa povezavam v grafu doloci cene. Vsaka povezava iz spodnjega seznama naj dobi svojo vrednost! Ce cen ne zelis prosim vpisi besedico 'NULL'"),
                htmlOutput("povezave_2"),
                textInput("utezi_2", "Vnesi celostevilske vrednosti, locene z vejico.", value="")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='9'",
                p("Lotil si se resevanja problema minimalne elektricne napeljave po mestih. Potreboval bom utezi na posameznih povezavah grafa."),
                htmlOutput("edges_3"),
                textInput("utezi_3", "Vnesi celostevilske vrednosti, locene z vejico.", value="")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='12'",
                p("Preveril bi rad graficnost poljubnega zaporedja stopenj vozlisc. Najprej doloci, ali preverjas graficnost za usmerjen ali neusmerjen graf."),
                selectInput("graficnost", "Izberi usmerjenost potencialnega grafa!",choices = c("usmerjen"=1,
                                                                                                "neusmerjen"=2,
                                                                                                "/"=3), selected =3),
                conditionalPanel(
                  condition= "input.graficnost == 1",
                  textInput("in_stopnje", "Vnesi zaporedje vhodnih stopenj, locenih z vejico.", value=""),
                  textInput("out_stopnje", "Vnesi zaporedje izhodnih stopenj, locenih z vejico.", value=""),
                  textOutput("preveri_stopnje")
                ),
                conditionalPanel(
                  condition= "input.graficnost == 2",
                  textInput("stopnje", "Vnesi zaporedje stopenj, locenih z vejico.", value="")
                )
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='13'",
                p("Preveril bi rad obstoj Eulerjevega cikla in Eulerjevega sprehoda v tvojem grafu? Povem ti lahko tudi, koliko truda moras vloziti da svoj graf narises..."),
                selectInput("Euler_izbor", "Izberi, kaj zelis da ti podam!",choices = list("Eulerjev cikel"=1,
                                                                                        "Eulerjev sprehod"=2,
                                                                                        "Min stevilo potez"=3,
                                                                                        "/"=4), selected =4),
                p("Vnesi stevilko vozlisca, kjer naj zacnem z iskanjem sprehoda/cikla."),
                textInput("Eu_start", "Izberi zacetno vozlisce.", value = "0")
              ),
              
              actionButton("button3", "Poslji poizvedbo")
              
            )),
            
            box( title ="Izbran graf:", status = "info",
                 plotOutput("graf_v_uporabi")
              
            ),
            
            
            box( title = "Rezultat poizvedbe", status = "success",
              
              conditionalPanel(
                condition = "input.characteristic == '1'",
                textOutput("text_4"),
                textOutput("poizvedba_st")
              ),
              conditionalPanel(
                condition = "input.characteristic == '2'",
                textOutput("text_5"),
                textOutput("ciklicnost")
              ),
              conditionalPanel(
                condition = "input.characteristic == '4'",
                textOutput("text_7"),
                plotOutput("poizvedba_dvo")
              ),
              conditionalPanel(
                condition = "input.characteristic == '7'",
                textOutput("text_10"),
                plotOutput("poizvedba_komp")
              ),
              conditionalPanel(
                condition = "input.characteristic == '3'",
                 textOutput("text_6")
              ),
              conditionalPanel(
                condition = "input.characteristic == '5'",
                textOutput("text_8")
              ),
              conditionalPanel(
                condition = "input.characteristic == '6'",
                textOutput("text_9")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='8'",
                textOutput("najceneje"),
                plotOutput("minimum_poti")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='9'",
                textOutput("min_cena"),
                plotOutput("napeljava")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='10'",
                textOutput("krom_num"),
                plotOutput("barvanje")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='11'",
                textOutput("planarity"),
                plotOutput("ravnina")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='12'",
                textOutput("text_11"),
                plotOutput("graf_iz_zaporedja")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='13'",
                textOutput("komentiraj"),
                textOutput("rezultat")
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='14'",
                plotOutput("povezavni"),
                p("Bi rad povezavni graf tvojega grafa uporabljal se naprej?"),
                selectInput("uporabi_line","Ali shranim graf?", choices = list("da" = 1,"ne" = 2), selected = 2)
              ),
              
              conditionalPanel(
                condition = "input.characteristic =='15'",
                plotOutput("komplementaren"),
                p("Bi rad komplementarni graf tvojega grafa uporabljal se naprej?"),
                selectInput("uporabi_kompl","Ali shranim graf?", choices = list("da" = 1,"ne" = 2), selected = 2)
              )

            )
            )),
    
    
    
    
    
    
    
    tabItem(tabName = "problemi",
            
            fluidRow(sidebarPanel(
              h3("MODELIRANJE PROBLEMA TRGOVSKEGA POTNIKA NA GRAFU"),
              box (width=12, title = "RESEVANJE", status = "primary",
          
                p("Za zacetek resevanja problema trgovskega potnika, moras povezavam dolociti utezi. Te lahko predstavljajo ceno voznje, porabljen cas, energijo, kolicino nafte,..."),
                textOutput("text_12"),
                htmlOutput("povezave_1"),
                textInput("utezi_1", "Vsaka povezava v povezavni matriki naj dobi svojo utez!", value=""),
                textOutput("text_13"),
                numericInput("start", "Vnesi vozlisce, kjer naj trgovski potnik zacne s potjo!", value=0, min=0),
                textOutput("text_14"),
                actionButton("button4", "Resitev")
            )),
            
            
            
            
            box(title = "RESITEV PROBLEMA", status = "success",
                textOutput("text_15"),
                verbatimTextOutput("TSP"),
                textOutput("text_16"),
                textOutput("pot")
                
            ))
    )
    ))
  
  






    
    
    















sidebar <- dashboardSidebar(hr(),
                            sidebarMenu(id="vnos",
                                        menuItem("Vnos grafa", tabName = "vnos", selected = TRUE)),
                            sidebarMenu(id="lastnosti", 
                                        menuItem("Ugotavljanje lastnosti grafa", tabName = "lastnosti")),
                            sidebarMenu(id="problemi", 
                                        menuItem("Izbira in resevanje problema", tabName = "problemi"))
)





ui <- fluidPage(useShinyjs(),
                dashboardPage(
                  dashboardHeader(title = "PROBLEMI NA GRAFIH"),
                  sidebar,
                  body,
                  skin = "blue"
                  ),
                theme = shinytheme("journal")
)



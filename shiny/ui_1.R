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
              #actionButton("posodobi1", "Posodobi"),
              box(width=12, title = "IZBOR LASTNOSTI", status = "primary",
              radioButtons("characteristic", 
                           h3("Izberi karakteristike, ki te zanimajo"),
                           choices = list("stopnje" = 1, 
                                          "vsebovanost ciklov" = 2,
                                          "stevilo povezav" = 3,
                                          "dvodelnost" = 4,
                                          "premer/diameter" = 5,
                                          "polmer/radij" = 6,
                                          "povezanost" = 7
                           )),
              
              actionButton("button3", "Poslji poizvedbo")
            )),
            
            box( title = "Rezultat poizvedbe", status = "success",
              textOutput("text_4"),
              conditionalPanel(
                condition = "input.characteristic == '1'",
                textOutput("poizvedba_st")
              ),
              conditionalPanel(
                condition = "input.characteristic == '2'",
                textOutput("ciklicnost")
              ),
              conditionalPanel(
                condition = "input.characteristic == '4'",
                plotOutput("poizvedba_dvo")
              ),
              conditionalPanel(
                condition = "input.characteristic == '7'",
                plotOutput("poizvedba_komp")
              ),
              conditionalPanel(
                condition = "input.characteristic == '3' | input.characteristic == '5' | input.characteristic == '6'",
                tabPanel("Poizvedba", uiOutput("poizvedba"))
              )

            )
            )),
    
    
    
    
    tabItem(tabName = "problemi",
            
            fluidRow(sidebarPanel(
              h3("MODELIRANJE PROBLEMOV NA GRAFIH"),
              #p("Klikni gumb 'Posodobi', da najprej osvezis podatke o grafu, ki ga preiskujes :)"),
              #actionButton("posodobi2", "Posodobi"),
              box (width=12, title = "IZBOR PROBLEMA", status = "primary",
              selectInput("problem", "Katerega od problemov se bos lotil?",
                          choices = list("Problem trgovskega potnika" = 1,
                                         "Barvanje grafa"=2,
                                         "Ravninskost grafa"=3,
                                         "Iskanje najcenejse poti"=4,
                                         "Problem minimalne elektricne napeljave"=5,
                                         "Problem ugasanja luci" = 6,
                                         "Graficnost zaporedja" = 7,
                                         "Eulerjeva lastnost" = 8,
                                         "Ustvari povezavni graf" = 9,
                                         "Ustvari komplementaren graf" = 10), selected = 3),
              
              conditionalPanel(
                condition = "input.problem =='1'",
                p("Izbral si problem trgovskega potnika, zato moras povezavam najprej dolociti utezi. Te lahko predstavljajo ceno voznje, porabljen cas, energijo, kolicino nafte,..."),
                textOutput("text_5"),
                htmlOutput("povezave_1"),
                textInput("utezi_1", "Vsaka povezava v povezavni matriki naj dobi svojo utez!", value=""),
                textOutput("text_6"),
                numericInput("start", "Vnesi vozlisce, kjer naj trgovski potnik zacne s potjo!", value=0, min=0),
                textOutput("text_7")
              ),
              
              conditionalPanel(
                condition = "input.problem =='2'",
                p("Te zanima kromaticno stevilo tvojega grafa? Pa poglejva...")
              ),
              
              conditionalPanel(
                condition = "input.problem =='3'",
                p("Te zanima ce je tvoj graf ravninski? Pa poglejva...")
              ),
              
              conditionalPanel(
                condition = "input.problem =='4'",
                p("Lotil si se iskanja najcenejse poti med dvema vozliscema. Najprej mi moras povedati kateri dve vozlisci naj gledam."),
                numericInput(inputId = "zacni", "Vnesi stevilko vozlisca, ki naj bo zacetek poti.", value = 0, min=0),
                numericInput(inputId = "finish", "Vnesi stevilko vozlisca, ki naj bo konec poti.", value = 0, min=0),
                p("Sedaj pa povezavam v grafu doloci cene. Vsaka povezava iz spodnjega seznama naj dobi svojo vrednost! Ce cen ne zelis prosim vpisi besedico 'NULL'"),
                htmlOutput("povezave_2"),
                textInput("utezi_2", "Vnesi celostevilske vrednosti, locene z vejico.", value="")
                ),
              
              conditionalPanel(
                condition = "input.problem =='5'",
                p("Lotil si se resevanja problema minimalne elektricne napeljave po mestih. Potreboval bom utezi na posameznih povezavah grafa."),
                htmlOutput("edges_3"),
                textInput("utezi_3", "Vnesi celostevilske vrednosti, locene z vejico.", value="")
              ),
              
              conditionalPanel(
                condition = "input.problem =='6'",
                p("Izbral si si igro ugasanja luci, ki jo bom modeliral s tvojim grafom. Najprej poglejva, ce igro na tvojem grafu sploh lahko igrava. Imeti mora namrec liho stevilo vozlisc, njegova povezavna matrika pa mora biti ustrezen zacetek igre."),
                actionButton("button6", "Preveri resljivost"),
                textOutput("solvable"),
                selectInput("nadaljevanje", "Bos nadaljeval igro na ze zaceti, si izbral drug graf ali ti igro generiram jaz?",
                            choices = list("nadaljuj"=1,
                                           "izbral bom drug graf"=2,
                                           "generiraj mi igro"=3,
                                           "/"= 4), selected = 4),
                textOutput("odlocitev"),
                conditionalPanel(
                  condition = "input.nadaljevanje =='3'",
                  numericInput(inputId = "dimenzija", "Vrednost naj bo eno od stevil 3, 5, 7 ali 9!", value =0, min=0),
                )
                
                #actionButton("button7", "Oglej si resitev")
              ),
              
              conditionalPanel(
                condition = "input.problem =='7'",
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
                condition = "input.problem =='8'",
                p("Preveril bi rad obstoj Eulerjevega cikla in Eulerjevega sprehoda v tvojem grafu? Povem ti lahko tudi, koliko truda moras vloziti da svoj graf narises..."),
                selectInput("Euler_izbor", "Izberi, kaj zelis da ti podam!",choices = c("Eulerjev cikel"=1,
                                                                                      "Eulerjev sprehod"=2,
                                                                                      "Min stevilo potez"=3,
                                                                                      "/"=4), selected =4),
                p("Vnesi stevilko vozlisca, kjer naj zacnem z iskanjem sprehoda/cikla."),
                textInput("Eu_start", "Izberi zacetno vozlisce.", value = "0")
                ),
              
              conditionalPanel(
                condition = "input.problem =='9'",
                p("Bi rad videl kaksen je pripadajoc povezavni graf tvojega grafa?")
              ),
              
              conditionalPanel(
                condition = "input.problem =='10'",
                p("Bi rad videl kaksen je pripadajoc komplementaren graf tvojega grafa?")
              ),

              actionButton("button4", "Resitev")
            )),
            
            
            
            
            box(title = "RESITEV PROBLEMA", status = "success",
              
              conditionalPanel(
                condition = "input.problem =='1'",
                textOutput("text_8"),
                verbatimTextOutput("TSP"),
                textOutput("text_9"),
                textOutput("pot")
                ),
              
              conditionalPanel(
                condition = "input.problem =='2'",
                textOutput("krom_num"),
                plotOutput("barvanje")
              ),
              
              conditionalPanel(
                condition = "input.problem =='3'",
                textOutput("planarity"),
                plotOutput("ravnina")
              ),
              
              conditionalPanel(
                condition = "input.problem =='4'",
                textOutput("najceneje"),
                plotOutput("minimum_poti")
                #textOutput("pogostost")
              ),
              
              conditionalPanel(
                condition = "input.problem =='5'",
                textOutput("min_cena"),
                plotOutput("napeljava")
              ),
              
              conditionalPanel(
                condition = "input.problem =='6'",
                #p("Ce si obupal mi sporoci.. Tu ti bom pokazal resitev igre."),
                #imageOutput("resitev")
                conditionalPanel(
                  condition = "input.nadaljevanje =='3'",
                  p("To je zacetna igra, ki jo resujes:"),
                  htmlOutput("zacetna_igra"),
                  actionButton("button8", "Zacni z igro")
                ),
                conditionalPanel(
                  condition = "input.nadaljevanje =='1'",
                  p("To je zacetna igra, ki jo resujes:"),
                  htmlOutput("matrika_grafa"),
                  actionButton("button9", "Zacni z igro")
                ),
                conditionalPanel(
                  condition = "input.button9 || input.button8",
                  p("Kje se nahaja zarnica, ki ji bos spremenil stanje?"),
                  numericInput("x", "vrstica", value = 0, min=0),
                  numericInput("y", "stolpec", value = 0, min=0),
                  p("Ko se odlocis, klikni gumb 'Spremeni', da preveris svojo odlocitev."),
                  actionButton("button11", "Spremeni")
                ),
                p("Trenutna stanja zarnic:"),
                htmlOutput("stanja")
              ),
              
              conditionalPanel(
                condition = "input.problem =='7'",
                textOutput("graf_zaporedje"),
                plotOutput("graf_iz_zaporedja")
              ),
              
              conditionalPanel(
                condition = "input.problem =='8'",
                textOutput("komentiraj"),
                textOutput("rezultat")
              ),
              
              conditionalPanel(
                condition = "input.problem =='9'",
                plotOutput("povezavni"),
                p("Bi rad shranil povezavni graf za raziskovanje naprej?"),
                actionButton("shrani1", "Shrani")
              ),
              
              conditionalPanel(
                condition = "input.problem =='10'",
                plotOutput("komplementaren"),
                p("Bi rad shranil komplementaren graf za raziskovanje naprej?"),
                actionButton("shrani2", "Shrani")
              )
              
              
              
              
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



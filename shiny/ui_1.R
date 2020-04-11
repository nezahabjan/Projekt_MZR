# tu je ui funkcija

body <- dashboardBody(
  tabItems(
    
    
    ### ZAVIHEK VNOS
    
    tabItem(tabName = "vnos",
            
            fluidRow(sidebarPanel(
              h3("Dobrodosel v svetu grafov!"),
              
              selectInput("generate", "Naj ti graf generiram jaz, ali imas ze izbranega? ",
                          choices = list("generiraj" = 1, "imam_svoj_graf"=2, "/"=3), selected = 3),
              
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
                
                actionButton("button2", "Vnesi elemente povezavne matrike"),
                
                textOutput("text_3"),
                uiOutput("vnos")
                
              ),
              

              selectInput("dir", "Izberi se usmerjenost grafa",
                          choices = list("Graf ni usmerjen" = 1, "Graf je usmerjen" = 2, "nevem" = 3), selected = 3),
              
              textOutput("text_1"),

              actionButton("button1", "Narisi")
              
            ),
            
            mainPanel( 
              tabPanel("Graf", plotOutput("graf"))
            ))),
    
    
    
    ### ZAVIHEK LASTNOSTI
    
    tabItem(tabName = "lastnosti",
            
            fluidRow(sidebarPanel(
              h3("Bi rad izvedel kaj o osnovnih lastnostih tvojega grafa?"),
              
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
            ),
            
            mainPanel(
              textOutput("text_4"),
              textOutput("ciklicnost"),
              tabPanel("Poizvedba", uiOutput("poizvedba"))

              
              
            )
            )),
    
    
    
    
    tabItem(tabName = "problemi",
            
            fluidRow(sidebarPanel(
              h3("Bi rad na svojem grafu modeliral kaksen problem?"),
              
              selectInput("problem", "Katerega od problemov se bos lotil?",
                          choices = list("Problem trgovskega potnika" = 1,
                                         "Barvanje grafa"=2,
                                         "Ravninskost grafa"=3,
                                         "Iskanje najcenejse poti"=4,
                                         "Problem minimalne elektricne napeljave"=5,
                                         "Problem ugasanja luci" = 6), selected = 3),
              
              conditionalPanel(
                condition = "input.problem =='1'",
                p("Izbral si problem trgovskega potnika, zato moras povezavam najprej dolociti utezi. Te lahko predstavljajo ceno voznje, porabljen cas, energijo, kolicino nafte,..."),
                textOutput("text_5"),
                htmlOutput("edges_1"),
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
                htmlOutput("edges_2"),
                textInput("utezi_2", "Vnesi vrednosti.", value="")
                ),
              
              conditionalPanel(
                condition = "input.problem =='5'",
                p("Lotil si se resevanja problema minimalne elektricne napeljave po mestih. Potreboval bom utezi na posameznih povezavah grafa."),
                htmlOutput("edges_3"),
                textInput("utezi_3", "Vnesi vrednosti.", value="")
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
              
              
              
              
              
              
              actionButton("button4", "Resi problem"),
              
              conditionalPanel(
                condition = "input.problem =='4'",
                p("Preveris lahko tudi, kolikokrat se med najkrajsimi potmi pojavi izbrano vozlisce"),
                numericInput(inputId = "pojav", "Vnesi stevilko vozlisca, katerega uporabnost te zanima.", value = 0),
                actionButton("button5", "Preveri pogostost")
               )
              
              
            ),
            
            mainPanel(
              
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
                textOutput("poti"),
                textOutput("pogostost")
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
                  actionButton("button8", "Zacni z igro"),
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
                  p("Ko se odlocis, klikni gumb 'Resi problem', da preveris svojo odlocitev.")
                ),
                p("Trenutna stanja zarnic:"),
                htmlOutput("stanja")
              
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
                  dashboardHeader(title = "Problemi na grafih"),
                  sidebar,
                  body,
                  skin = "yellow"),
                theme="bootstrap.css"
)



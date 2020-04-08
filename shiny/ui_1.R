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
              tabPanel("Poizvedba", uiOutput("poizvedba"))

              
              
            )
            )),
    
    
    
    
    tabItem(tabName = "problemi",
            
            fluidRow(sidebarPanel(
              h3("Bi rad na svojem grafu resil kaksen problem?"),
              
              selectInput("problem", "Katerega od problemov se bos lotil?",
                          choices = list("Problem trgovskega potnika" = 1,"/"=2), selected = 2),
              
              
              conditionalPanel(
                condition = "input.problem =='1'",
                p("Izbral si problem trgovskega potnika, zato moras povezavam najprej dolociti utezi. Te lahko predstavljajo ceno voznje, porabljen cas, energijo, kolicino nafte,..."),
                
                textOutput("text_5"),
                htmlOutput("edges"),
                
                textInput("utezi", "Vsaka povezava v povezavni matriki naj dobi svojo utez!", value=""),

                textOutput("text_6"),
                numericInput("start", "Vnesi vozlisce, kjer naj trgovski potnik zacne s potjo!", value=0, min=0),

                textOutput("text_7")
              ),
              
              actionButton("button4", "Resi problem")
              
              
            ),
            
            mainPanel(
              textOutput("text_8"),
              verbatimTextOutput("TSP"),
              textOutput("text_9"),
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
                  dashboardHeader(title = "Problemi na grafih"),
                  sidebar,
                  body,
                  skin = "yellow"),
                theme="bootstrap.css"
)



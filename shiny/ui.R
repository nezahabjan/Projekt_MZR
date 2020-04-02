source("../libraries/lib.R")
source("../glavne funkcije/funkcije.R")
source("server.R")




## UI funkcija
sidebar <- dashboardSidebar(
                            sidebarMenu(id="vnos",
                                        menuItem("Vnos grafa", tabName = "vnos", selected = TRUE)),
                            sidebarMenu(id="lastnosti", 
                                        menuItem("Ugotavljanje lastnosti grafa", tabName = "lastnosti")),
                            sidebarMenu(id="problemi", 
                                        menuItem("Izbira in reševanje problema", tabName = "problemi"))
                            )

body <- dashboardBody(
  tabItems(

    tabItem(tabName = "vnos",
            
            fluidRow(sidebarPanel(
              h3("Dobrodosli v svetu grafov!"),
              
              selectInput("generate", "Zelis, da ti ponudim nakljucen graf po izbranih atributih?",
                          choices = list("Ne, graf imam ze izbran" = 1,"Da, prosim" = 2, "/"=3), selected = 3),
              textOutput("text_2"),
              
              selectInput("dir", "Izberi usmerjenost grafa",
                          choices = list("Graf ni usmerjen" = 1, "Graf je usmerjen" = 2, "nevem" = 3), selected = 3),
              textOutput("text_1"),
              
          
          # v primeru da zelimo zgeneriran neusmerjen graf, moramo podati stevilo zeljenih oglisc        
          conditionalPanel(
                condition = "input.dir == '2' && input.generate == '2'",
                numericInput(inputId = "ogl",
                             label = "Izberi stevilo oglisc grafa",
                             value = 0 ),
                ),
          
          conditionalPanel(
            condition = "input.ogl == '0' && input.button1 == 'TRUE'",
            p("Prosim, preveri ponovno izbrano stevilo oglisc!")
          ),
          
          
          # v primeru, da imamo graf ze izbran, programu podamo dimenzije povezavne matrike
          conditionalPanel(
            condition = "input.generate == '1'",
            numericInput(inputId = "Dim_x_1",
                         label = "Izberi x dimenzijo povezavne matrike",
                         value = 0 ),
            
            numericInput(inputId = "Dim_y_1",
                         label = "Izberi y dimenzijo povezavne matrike",
                         value = 0 ),
            
            actionButton("button2", "Vnesi elemente")
   
          ),
          

          # v primeru, da zelimo zgeneriran usmerjen graf, podamo dimenziji povezavne matrike
          conditionalPanel(
            condition = "input.dir == '1' && input.generate == '2'",
            p("V redu, potreboval bom le zeljeni dimenziji povezavne matrike"),
            numericInput(inputId = "Dim_x_2",
                         label = "Izberi x dimenzijo",
                         value = 0 ),
            
            numericInput(inputId = "Dim_y_2",
                         label = "Izberi y dimenzijo",
                         value = 0 )
          ),

              actionButton("button1", "Narisi")
              
            ),
            
            mainPanel( 
              
              textOutput("text_3"),
              uiOutput("vnos"),
              tabPanel("Graf", plotOutput("graf"))
                      
            ))),
    
    
    
    
    
    
    
    tabItem(tabName = "lastnosti",
            
            fluidRow(sidebarPanel(
              h3("Bi rad izvedel kaj o osnovnih lastnostih tvojega grafa?"),

              radioButtons("characteristic", h3("Izberi karakteristike, ki te zanimajo"),
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
              textOutput("text_5"),
              uiOutput("cikli"),
              textOutput("stopnje"),
              textOutput("text_6"),
              plotOutput("dvodelnost")
              
              
            )
            )),
    
    
    
    
    
    
    tabItem(tabName = "problemi",
            
            fluidRow(sidebarPanel(
              h3("Bi rad na svojem grafu resil kaksen problem?"),
              
              selectInput("problem", "Kaj bi rad pocel s svojim grafom?",
                          choices = list("Problem trgovskega potnika" = 1,"Iskanje najkrajse poti" = 2, "Preverjanje dvodelnosti"=3, "/"=4), selected = 4),

              
              conditionalPanel(
                condition = "input.problem =='1'",
                 p("Izbral si problem trgovskega potnika, za resevanje moras povezavam dolociti utezi"),
                
                matrixInput("utezi", matrix(0,4,5), class="numeric")
                
                
              ),
              actionButton("button4", "Resi problem")
              
              
            ),
            
            mainPanel(
              uiOutput("dimenzije"),
              textOutput("text_6")
              
              
            ))
    ))


)





ui <- fluidPage(useShinyjs(),
                    dashboardPage(
                    dashboardHeader(title = "Problemi na grafih"),
                    sidebar,
                    body,
                    skin = "yellow"),
            theme="bootstrap.css"
)
shinyApp(ui=ui, server=server) 






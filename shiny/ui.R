#source("../libraries/lib.R")

#knjiznice, ki jih potrebujemo
library(igraph)
library(ggraph)
library(networkD3)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyMatrix)
library(TSP)
library(pracma)
library(randomGLM)
library(googleVis)


#datoteke, na katere se sklicujemo v aplikaciji
source("../glavne funkcije/funkcije.R")
source("server.R")


## UI funkcija
sidebar <- dashboardSidebar(hr(),
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
                                 numericInput(inputId = "vozl_1",
                                              label = "Izberi stevilo vozlisc grafa",
                                              value = 0 ),
                                 numericInput(inputId = "povez",
                                              label = "Izberi stevilo povezav grafa",
                                              value = 0 ))
              ),
              
              
              conditionalPanel(
                condition = "input.generate == '2'",
                numericInput(inputId = "vozl_2",
                             label = "Izberi stevilo vozlisc grafa",
                             value = 0 ),
                actionButton("button2", "Vnesi elemente povezavne matrike")
                
              ),
              
              
              
              selectInput("dir", "Izberi se usmerjenost grafa",
                          choices = list("Graf ni usmerjen" = 1, "Graf je usmerjen" = 2, "nevem" = 3), selected = 3),
              
              textOutput("text_1"),
              
              
              
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

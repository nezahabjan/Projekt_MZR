
library(shiny)
library(shinyMatrix)



## SERVER funkcija
server <- function(input, output, session) {
  
  
  #najprej cakamo, da uporabnik izbere usmerjenost grafa in nato mu svetujemo, katere
  #atribute grafa mora vnesti
  observeEvent( input$dir, {
    
    if (input$dir == "yes"){
      output$text_1 <- renderText({ "Graf bo usmerjen, torej doloci se stevilo oglisc!" })
    } else {
      output$text_1 <- renderText({ "Graf bo neusmerjen, torej doloci dimenziji povezavne matrike!" }) 
    }
  })
  

  observeEvent( input$generate, {
    
    if (input$generate == "yes"){
      output$text_2 <- renderText({ " Le se en klik te loci od dobljenega grafa... " })
    } else {
      output$text_2 <- renderText({ " Sedaj moras vnesti se povezavno matriko svojega grafa :) " })
    }
      
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent( input$button1, {
    
    if (input$generate == "yes"){
      
      output$graf <- renderPlot({
        net <- narisi("", input$dir, input$ogl, input$Dim_x, input$Dim_y)$network
        plot.igraph(net)
        
      })
      
    
    } else {

      matrika <- as.matrix(lapply(1:input$Dim_x, function(i) 
        as.numeric(unlist(strsplit(input[[paste0("v",i)]])))))
      
      output$graf <- renderPlot({
        net <- narisi(matrika, input$dir, input$ogl, input$Dim_x, input$Dim_y)$network
        plot.igraph(net)
        
      })

    }
  })
}


## UI funkcija
sidebar <- dashboardSidebar(hr(),
                            sidebarMenu(id="atributi",
                                        menuItem("Atributi", tabName = "atributi", selected = TRUE)),
                            sidebarMenu(id="lastnosti", 
                                        menuItem("Ugotavljanje lastnosti grafa", tabName = "lastnosti")),
                            sidebarMenu(id="problemi", 
                                        menuItem("Izbira in reševanje problema", tabName = "problemi"))
                            )

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "atributi",
            
            fluidRow(sidebarPanel(
              h3("Dobrodosli v svetu grafov!"),
              
              selectInput("dir", "Izberi usmerjenost grafa",
                          choices = list("yes" = "yes", "no" = "no")),
              
              textOutput("text_1"),
              
              numericInput(inputId = "Dim_x",
                           label = "Izberi x dimenzijo matrike",
                           value = 4 ),
              
              numericInput(inputId = "Dim_y",
                           label = "Izberi y dimenzijo matrike",
                           value = 5 ),
              
              numericInput(inputId = "ogl",
                           label = "Izberi stevilo oglisc grafa",
                           value = 6 ),
              
              selectInput("generate", "Zelis, da ti ponudim graf?",
                          choices = list("no" = "no","yes" = "yes")),
              
              textOutput("text_2"),
              
              actionButton("button1", "Submit")
              
            ),
            
            mainPanel( 
              
              tabPanel("Graf", plotOutput("graf"))
                      
            ))),
    
    
    tabItem(tabName = "lastnosti",
            
            fluidRow(sidebarPanel(
              h3("Bi rad izvedel kaj o osnovnih lastnostih tvojega grafa?")

            )),
            mainPanel(p("Tu so rezultati!")
            )),
    
    
    tabItem(tabName = "problemi",
            
            fluidRow(sidebarPanel(
              h3("Bi rad na svojem grafu resil kaksen problem?")
              
            )),
            mainPanel(p("Tu so rezultati!")
            ))
    ))



ui <- fluidPage(useShinyjs(),
                    dashboardPage(
                    dashboardHeader(title = "Problemi na grafih"),
                    sidebar,
                    body,
                    skin = "yellow"),
            theme="bootstrap.css"
)
shinyApp(ui=ui, server=server) 






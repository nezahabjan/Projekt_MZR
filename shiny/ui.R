
library(shiny)
library(shinyMatrix)



## SERVER funkcija
server <- function(input, output, session) {
  
  
  #najprej cakamo, da uporabnik izbere usmerjenost grafa in nato mu svetujemo, katere
  #atribute grafa mora vnesti
  observeEvent( input$dir, {
    
    if (input$dir == "yes"){
      output$text_1 <- renderText({ "Graf bo usmerjen, torej doloci se stevilo oglisc!" })
    } else if (input$dir == "no"){
      output$text_1 <- renderText({ "Graf bo neusmerjen, torej doloci dimenziji povezavne matrike!" }) 
    } else {
      output$text_1 <- renderText({ "Preden nadaljujes moras dolociti usmerjenost grafa!" }) 
    }
  })
  

  observeEvent( input$generate, {
    
    if (input$generate == "yes"){
      output$text_2 <- renderText({ "Super, podati mi moras le izbrane atribute grafa!" })
    } else if (input$generate == "no"){
      output$text_2 <- renderText({ "Sedaj moras vnesti se povezavno matriko svojega grafa :)" })
    } else {
      output$text_2 <- renderText({ "Prosim, izpolni zgornje polje!" })
    }
      
  })
  

  
  observeEvent( input$button1, {
    
    if (input$generate == "yes"){
      
      output$graf <- renderPlot({
        net <- narisi("", input$dir, input$ogl, input$Dim_x, input$Dim_y)$network
        plot.igraph(net)
        
      })
      
    
    } else if (input$generate == "no"){

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
              
              
              selectInput("generate", "Zelis, da ti ponudim nakljucen graf po izbranih atributih?",
                          choices = list("no" = "no","yes" = "yes", "izberi"="izberi"), selected = "izberi"),
              
              textOutput("text_2"),
              
              
              selectInput("dir", "Izberi usmerjenost grafa",
                          choices = list("yes" = "yes", "no" = "no", "nevem" = "nevem"), selected = "nevem"),
              
              textOutput("text_1"),
              
              
          conditionalPanel(
                condition = "input.dir == 'yes' || input.generate == 'yes'",
                numericInput(inputId = "ogl",
                             label = "Izberi stevilo oglisc grafa",
                             value = 0 )
                ),
          
          conditionalPanel(
            condition = "input.dir == 'no'",
            numericInput(inputId = "Dim_x",
                         label = "Izberi x dimenzijo matrike",
                         value = 0 ),
            
            numericInput(inputId = "Dim_y",
                         label = "Izberi y dimenzijo matrike",
                         value = 0 )
          ),

          
          conditionalPanel(
            condition = "input.generate == 'no'",
            lapply((1:2), function(i) {
              textInput(paste0("v",i), paste0("v",i), "0,1,2")})

          ),
              
          
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






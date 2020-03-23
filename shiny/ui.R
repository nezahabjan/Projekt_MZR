
library(shiny)
library(shinyMatrix)



## SERVER funkcija
server <- function(input, output, session) {
  
  
  #najprej cakamo, da uporabnik izbere usmerjenost grafa in nato mu svetujemo, katere
  #atribute grafa mora vnesti
  observeEvent( input$dir, {
    
    if (input$dir == 2){
      output$text_1 <- renderText({ "Graf bo usmerjen!" })
    } else if (input$dir == 1){
      output$text_1 <- renderText({ "Graf bo neusmerjen!" }) 
    } else {
      output$text_1 <- renderText({ "Prosim, izpolni zgornje polje!" }) 
    }
  })
  

  observeEvent( input$generate, {
    
    if (input$generate == 2){
      output$text_2 <- renderText({ "Super, podati mi moras le izbrane atribute grafa!" })
    } else if (input$generate == 1){
      output$text_2 <- renderText({ "Torej moras vnesti se povezavno matriko svojega grafa :)" })
    } else {
      output$text_2 <- renderText({ "Prosim, izpolni zgornje polje!" })
    }
      
  })
  
  
  
  observeEvent( input$button2, {
    if (input$Dim_x_1 > 0 & input$Dim_y_1 > 0){
    
    p("Sedaj lahko vneses elemente povezavne matrike")
    
      lapply((1:input$Dim_x_1), function(i) {
      textInput(paste0("v",i), paste0("v",i), "0,1,2")})
    
    } else {
      
      p("Ponovno izpolni dimenziji matrike")
      
    }
  })
    
  

  
  observeEvent( input$button1, {
    
    if (input$generate == 2){
      
      output$graf <- renderPlot({
        net <- narisi("", input$dir, input$ogl, input$Dim_x, input$Dim_y)$network
        plot.igraph(net)
        
      })
      
    
    } else if (input$generate == 1){

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
                          choices = list("Ne, graf imam ze izbran" = 1,"Da, prosim" = 2, "/"=3), selected = 3),
              
              textOutput("text_2"),
              
              
              selectInput("dir", "Izberi usmerjenost grafa",
                          choices = list("Graf ni usmerjen" = 1, "Graf je usmerjen" = 2, "nevem" = 3), selected = 3),
              
              textOutput("text_1"),
              
              
          conditionalPanel(
                condition = "input.dir == '2' && input.generate == '2'",
                numericInput(inputId = "ogl",
                             label = "Izberi stevilo oglisc grafa",
                             value = 0 )
                ),
          
          
          
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






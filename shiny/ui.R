
library(shiny)
library(shinyMatrix)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Lastnosti grafov in problemi na njih"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      selectInput("dir", "Izberi usmerjenost grafa",
                  choices = list("yes" = "yes", "no" = "no")),
      
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
      
      conditionalPanel("Vnesi elemente matrike!",
        "input$Dim_x>=5",
        lapply((1:"input$Dim_x"), function(i) {
          #print("input.Dim_x")
        textInput(paste0("v",i), paste0("v",i), "0,1,2")
          
          }))
        ,
      
      actionButton("button1", "Submit")

      ),

      
      
    mainPanel(

       tabPanel("Graf", plotOutput("graf"),
                plotOutput("graf_2"))
                
                
                
      )
      
      
      
    )
  #shinyApp(ui=ui, server=server)  
  )
  
















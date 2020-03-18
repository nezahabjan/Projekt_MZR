
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
      
      # Input: Slider for the number of bins ----
      numericInput(inputId = "Dim_x",
                  label = "Izberi x dimenzijo matrike",
                  value = 4 ),
      
      numericInput(inputId = "Dim_y",
                   label = "Izberi y dimenzijo matrike",
                   value = 5 ),
      
      numericInput(inputId = "ogl",
                   label = "Izberi stevilo oglisc grafa",
                   value = 6 ),
      
      
      selectInput("dir", h3("Izberi usmerjenost grafa"),
                   choices = list("nevem" = "nevem"," yes" = "yes", "no" = "no")
                                  ),
      
      actionButton("button1", "Submit")

      ),

      
      
    mainPanel(

       tabPanel("Graf", plotOutput("graf"))
      )
      
      
      
    )
  #shinyApp(ui=ui, server=server)  
  )
  
















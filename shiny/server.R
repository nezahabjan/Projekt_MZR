
server <- function(input, output, session) {
  

  #najprej cakamo, da uporabnik izbere usmerjenost grafa in nato mu svetujemo, katere
  #atribute grafa mora vnesti
  observeEvent( input$dir, {
    
    if (input$dir == "yes"){
    print("Doloci stevilo oglisc")
    } else {
    print("Doloci dimenziji povezavne matrike")  
    }
    
  })
  
  
  
  observeEvent( input$button1, {
    
    if (input$generate == "yes"){
      
        print("Tu je poljuben graf po izbranih atributih!")
        output$graf <- renderPlot({
          
          net <- narisi("", input$dir, input$ogl, input$Dim_x, input$Dim_y)$network
          
          plot.igraph(net)
          
          
        })
      
      
      
      
    } else {
      
      print("Vnesi se povezavno matriko tvojega grafa!")
      
      
      matrika <- as.matrix(lapply(1:input$Dim_x, function(i) 
        as.numeric(unlist(strsplit(input[[paste0("v",i)]])))))
      
      output$graf_2 <- renderPlot({
        
        net <- narisi(matrika, input$dir, input$ogl, input$Dim_x, input$Dim_y)$network
        
        plot.igraph(net)
        
        
      })

      

      
    
    
    }
  

})
   
    
}
  
  
  

  
  
  
  
  




### to je glavna skripta za zagon aplikacije

#knjiznice, ki jih potrebujemo
library(RBGL)
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
library(colormap)
library(colortools)
library(lpSolve)
library(DiagrammeR)
library(lightsout)
library(hash)




#datoteke, na katere se sklicujemo v aplikaciji
source("../glavne funkcije/funkcije.R")
source("server_1.R")
source("ui_1.R")


#aplikacija shiny
shinyApp(ui=ui, server=server) 
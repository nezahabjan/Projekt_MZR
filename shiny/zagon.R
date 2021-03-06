### to je glavna skripta za zagon aplikacije

#knjiznice, ki jih potrebujemo
library(RBGL)
library(igraph)
library(networkD3)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyMatrix)
library(TSP)
library(pracma)
library(randomGLM)
library(googleVis)
library(colortools)
library(lpSolve)
library(DiagrammeR)
library(hash)
library(PairViz)
library(eulerian)
library(shinythemes)



#datoteke, na katere se sklicujemo v aplikaciji
source("../glavne funkcije/funkcije.R", encoding = "UTF-8")
source("server.R", encoding = "UTF-8")
source("ui.R", encoding = "UTF-8")


#aplikacija shiny
shinyApp(ui, server) 
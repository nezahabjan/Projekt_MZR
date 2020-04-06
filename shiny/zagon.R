### to je glavna skripta za zagon aplikacije


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
source("server_1.R")
source("ui_1.R")


#aplikacija shiny
shinyApp(ui=ui, server=server) 
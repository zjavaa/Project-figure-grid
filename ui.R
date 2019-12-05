#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("D:/Uczelnia/projektostwersja/projekt/funkcje.R")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    column(6,
           # Funckja służąca do załadowania pliku
           fileInput("file", label = h3("Wczytaj figure") , multiple = FALSE , buttonLabel = "Wczytaj" ))),

  # Show a plot of the generated distribution
  fluidRow(column(width=6,
                  # verbatimTextOutput("tekst")
                         rglwidgetOutput("plot3d",height = 500)


  ),
    column(width=6,
                    plotOutput('siatka',height = 600)
                   )
  )
  )
)


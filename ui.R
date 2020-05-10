#ui
#
library(shiny)
library(plotly)
library(coronavirus)


shinyUI(fluidPage(
  headerPanel("Analiza danych- koronawirus"),
  
  sidebarPanel(
    selectInput(inputId = "Kraj",
                label = "Wybierz kraj",
                choices = unique(coronavirus$Country.Region),
                selected = "Poland")
  ),
    
    sidebarPanel(
      selectInput(inputId = "Region",
                  label = "Wybierz Region/Prowincję/Stan",
                  choices = unique(coronavirus$Province.State))
  ),
  
  sidebarPanel(
    p("Wybierz rodzaj danych: "),
    checkboxInput("confirmed", "Potwierdzone przypadki", FALSE),
    checkboxInput("death", "Zmarli", FALSE),
    checkboxInput("recovered", "Ozdrowiali", FALSE),
    checkboxInput("Daily.change.in.total.tests", "Ilość testów", FALSE)),


))

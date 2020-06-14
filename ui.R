#ui
library(shiny)
library(ggplot2)
library(coronavirus)
source('covid-maps.R')
source('Predykcja.R')
library(shinydashboard)
library(leaflet)
require("GGally")
source('testy.R')
#require("data sets")

paragraphText = 'Powyżej mapa przedstawiająca aktualną liczbę  skumulowanych przypadków zachorowań na Covid-19'


sidebarUI <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Mapa", tabName = "Mapa", icon = icon("globe-africa")),
    menuItem("Polska", tabName = "Polska", icon = icon("chart-bar")),
    menuItem("Swiat", tabName = "Swiat", icon = icon("atlas")),
    menuItem("Predykcja", tabName = "Predykcja", icon = icon("chart-line")),
    menuItem("Potwór spagetti", tabName = "Kitku", icon = icon("pastafarianism")),
    menuItem("Informacje", tabName = "Informacje", icon = icon("thumbtack")),
    
  collapsed = FALSE)
)


dashboardUI <- dashboardBody(
  tabItems(
    tabItem(tabName = "Mapa",
            fluidRow(
              
    

      titlePanel(title="Aktualna mapa obrazująca sumaryczną ilość osób zakażonych"),
  
                 sliderInput("chosenDate",
                             "Wybierz datę:",
                             min = as.Date("2020-01-01","%Y-%m-%d"),
                             max = Sys.Date() - 1,
                             value = Sys.Date() - 1,
                             timeFormat="%Y-%m-%d"),
      
               leafletOutput("covMap", height = 800, width = 800),
               p(paragraphText),
               p(textOutput(outputId = 'tekst1'))
              
            )
    ),
    tabItem(tabName = "Swiat",
            fluidRow(
              titlePanel(title="Analiza aktualnych danych dla Świata"),
              sidebarPanel(
               selectInput(inputId = "Kraj",
                           label = "Wybierz kraj",
                           choices = unique(coronavirus$Country.Region),
                           selected = "Germany")
               )
              )
            ),
    tabItem (tabName="Polska",
             fluidRow(
             titlePanel(title="Analiza aktualnych danych dla Polski"),
             tabBox(
               tabPanel("Analiza danych",
                        p("Wybierz rodzaj danych: "),
                        checkboxInput("confirmed", "Potwierdzone przypadki", FALSE),
                        checkboxInput("death", "Zmarli", FALSE),
                        checkboxInput("recovered", "Ozdrowiali", FALSE),
                        checkboxInput("testy", "Ilość testów", FALSE),
                        plotOutput('ploty'),
                      ),
               tabPanel("Ilości na dzień",
               p("Wybierz rodzaj danych: "),
               checkboxInput("confirmedD", "Potwierdzone przypadki", FALSE),
               checkboxInput("deathD", "Zmarli", FALSE),
               checkboxInput("recoveredD", "Ozdrowiali", FALSE),
               checkboxInput("testyD", "Ilość testów", FALSE),
               plotOutput('plotyDzien')),
               
               tabPanel("Analiza 3"))
             )
             
             
            
    ),
    tabItem(tabName = "Predykcja",
            fluidRow(
              titlePanel(title="Predykcja zachorowań w Polsce"),
              sidebarPanel(
              plotOutput("plot1"))
            )
    ),
    
    tabItem(tabName = "Informacje",
            fluidPage(
              titlePanel(title="Informacje"),
              sidebarPanel(uiOutput("tab"),
              uiOutput("tab2"),
              uiOutput("tab3"),
              imageOutput("IM1"))
              
            )
    )
  )
)
             

    





####################################################

ui <- dashboardPage(
  dashboardHeader(title = 'CoVid-19'),
  sidebarUI,
  dashboardUI
  
)

#shinyUI(fluidPage(
#  headerPanel("Analiza danych- koronawirus"),
#  
#  sidebarPanel(
#    selectInput(inputId = "Kraj",
#                label = "Wybierz kraj",
#                choices = unique(coronavirus$Country.Region),
#                selected = "Poland")
# ),
#    
#    sidebarPanel(
 #     selectInput(inputId = "Region",
#                  label = "Wybierz Region/Prowincję/Stan",
#                  choices = unique(coronavirus$Province.State))
 # ),
  
  #sidebarPanel(
  #  p("Wybierz rodzaj danych: "),
  #  checkboxInput("confirmed", "Potwierdzone przypadki", FALSE),
  # checkboxInput("death", "Zmarli", FALSE),
   # checkboxInput("recovered", "Ozdrowiali", FALSE),
  #  checkboxInput("Daily.change.in.total.tests", "Ilość testów", FALSE)),
# br(),
# mainPanel(
 #  tabsetPanel(
#     tabPanel("Histogram", plotOutput("wyjscieHistogram"))
 #  ))
#))
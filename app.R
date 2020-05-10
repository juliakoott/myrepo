# ! encoding: utf-8


# Niemozna uzywac polskich znakow
# Demonstracja mapy w prostej aplikacji shiny 

# wczesniej wszystko bylo w projekcie wiec w w razie problemow z wczytaniem
# trzeba nadac biezacy katalog z setwd() lub otworzyc w projekcie
source('covid-maps.R')

library(shiny)
library(shinydashboard)
library(leaflet)

paragraphText = 'Powyzej mapa przedstawiająca aktualną liczbę  skumulowanych przypadków zachorowań na Covid-19'

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(leafletOutput("mymap", height = 800),p(paragraphText))
    #actionButton("recalc", "New points")
)


# ui <- fluidPage(
#     leafletOutput("mymap")
# )

server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
        renderCovidMap() 
        # renderCovidMap()  %>% setView(42, 40, 4)
        # map <- leaflet() %>% addTiles()
    })
    

}

shinyApp(ui, server)
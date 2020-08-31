# ! encoding: utf-8

library(shiny)              #installed 24.08.20
library(shinydashboard)     #installed 24.08.20
library(leaflet)            #installed 24.08.20

# DODATKI
library("tmap")             #installed 24.08.20
library(leaflet.providers)  #installed 24.08.20

library(coronavirus)


# paragraphText = 'Powyzej mapa przedstawiająca aktualną liczbę  skumulowanych przypadków zachorowań na Covid-19'


sidebarUI <- dashboardSidebar(
    sidebarMenu(
        menuItem("Mapa", tabName = "Mapa", icon = icon("globe-africa")),
        menuItem("Polska", tabName = "Polska", icon = icon("chart-bar")),
        menuItem("Swiat", tabName = "Swiat", icon = icon("atlas")),
        menuItem("Predykcja", tabName = "Predykcja", icon = icon("chart-line")),
        menuItem("Potwór spagetti", tabName = "Kitku", icon = icon("pastafarianism")),
        menuItem("Informacje", tabName = "Informacje", icon = icon("thumbtack"))
    ),
    collapsed = FALSE
)


dashboardUI = dashboardBody(
    tabItems(
        # ===== MAPY =====
        tabItem(tabName = "Mapa",
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mapStyles.css")), # CSS DO MAPY !WAZNE
                fluidRow(
                # style = "padding: 0; margin: 0;",
                # div(
                leafletOutput("covMap", height = '1000px'),
                fixedPanel(
                    id="on_map_box", #fixedPanel
                    class="on_map_box",
                    right=40, top=80, #width= '200px',
                    # draggable = TRUE, 
                    textOutput(outputId = 'mapText_info'),
                    sliderInput(
                        "chosenDate",
                        "Wybierz datę:",
                        min = as.Date("2020-02-01","%Y-%m-%d"),
                        max = Sys.Date() - 1,
                        value = Sys.Date() - 1,
                        timeFormat="%Y-%m-%d"),
                    p(textOutput(outputId = 'mapText_chosenDate')),
                    selectInput(inputId = "chosenDataType", label = "Wybierz dane", choices = c("kumulatywne","aktywne", "zgony"))
                )
            # )
        )), 
        # ===== MAPY KONIEC =====
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
            )
        )
)



ui <- dashboardPage(
    dashboardHeader(title = 'Mapa Covid-19'),
    sidebarUI,
    dashboardUI
)


shinyUI(ui)

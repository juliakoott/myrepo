# ! encoding: utf-8

library(shiny)              #installed 24.08.20
library(shinydashboard)     #installed 24.08.20


#   ----    MAPY:  Biblioteki, Pliki, Funkcje ,Stale  ----
library(shinyjs)            # !!
library(leaflet)            #installed 24.08.20
library("tmap")             #installed 24.08.20

# !Musi byc tu : 
source("mapsFiles/polish-Covid19-data.R", encoding = "UTF-8") 
updateCovid19PL()
# updateCovid19PL() Musi byc na poczatku  w ui.R inaczej wystapi blad w
# sliderInput() dla daty dla map Polski poniewaz dane  wczytywane z
# updateCovid19PL() zawieraja maksymalna dostepna date dla danych polskich 

#   ----  END:  MAPY  Biblioteki, Pliki, Funkcje  ----


library(coronavirus)




sidebarUI <- dashboardSidebar(
	sidebarMenu(
	  # menuItem("Mapa Polska", tabName = "MapaPolska", icon = icon("globe-africa")),
    menuItem("Mapa", tabName = "Mapa", icon = icon("globe-africa")),
    menuItem("Polska", tabName = "Polska", icon = icon("chart-bar")),
    menuItem("Swiat", tabName = "Swiat", icon = icon("atlas")),
    menuItem("Predykcja",	tabName = "Predykcja",	icon = icon("chart-line")),
    menuItem(	"Potwór spagetti",	tabName = "Kitku",	icon = icon("pastafarianism")),
    menuItem("Informacje",	tabName = "Informacje",	icon = icon("thumbtack"))
	),
	collapsed = FALSE
)


dashboardUI = dashboardBody(
  tabItems(
    # ===== MAPY =====
    tabItem(
      useShinyjs(), # WAZNE
      tabName = "Mapa",
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mapStyles.css")), # CSS DO MAPY !WAZNE
      # ===== Swiat =====
      tabsetPanel(
        tabPanel(
          "Świat",
          fluidRow(
            leafletOutput("covMap", height = '1000px'),
            div(
              id = "on_map_box", # fixedPanel
              class = "on_map_box",
              # right = 15, top = 55, 
              # draggable = TRUE, 
              div(
                id = "map_hideBox",
                textOutput(outputId = 'mapText_info'),
                sliderInput(
                  "chosenDate",
                  "Wybierz datę:",
                  min = as.Date("2020-02-01", "%Y-%m-%d"),
                  max = Sys.Date() - 1,
                  value = Sys.Date() - 1,
                  timeFormat = "%Y-%m-%d"
                ), 
                p(textOutput(outputId = 'mapText_chosenDate')),
                selectInput(inputId = "chosenDataType", label = "Wybierz dane", choices = c("kumulatywne","aktywne", "zgony"))
              ),
              # uiOutput("")
              actionButton(class = "map_showButton",inputId = "mapWorld_showButton", label = "Ukryj")
            )#
          )
        ),
        # ===== Polska =====
        tabPanel(
          title = "Polska", 
          tabName = "MapaPolska",
          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mapStyles.css")), # CSS DO MAPY !WAZNE
          fluidRow(
            leafletOutput("covMap_PL", height = '1000px'),
            div(
              id = "on_map_box_PL", # fixedPanel
              class = "on_map_box",
              # right = 15, top = 55, 
              # draggable = TRUE, 
              div(
                id = "map_PL_hideBox",
                p(id = "mapText_info_PL", "Aktualna mapa obrazująca sumaryczną liczbę przypadków zachorowań na Covid-19"),
                # textOutput(outputId = 'mapText_info_PL'), # OK
                sliderInput(
                  "chosenDate_PL", # OK
                  "Wybierz datę:", # OK
                  min = as.Date("2020-02-01", "%Y-%m-%d"),
                  max = Sys.Date() - 1,
                  value = .Covid19PL_DataObj$info$date - 5,
                  timeFormat = "%Y-%m-%d"
                ), 
                p(textOutput(outputId = 'mapText_chosenDate_PL')),
                # p(textOutput(outputId = 'mapText_chosenDate_PL')),
                p(id = "mapText_chosenDate_PL", htmltools::HTML("<b>Jesli dane są niewidoczne poruszaj suwakiem</b>"))
                # ,selectInput(inputId = "chosenDataType_PL", label = "Wybierz dane", choices = c("kumulatywne", "zgony"))
              ),
              actionButton(class = "map_showButton", inputId = "mapPL_showButton", label = "Ukryj")
            )
          )
        )
        #  ~~~~~~  END: Polska  ~~~~~~
      )
    ), 
    # ===== End: MAPY =====
    tabItem(
      tabName = "Swiat",
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mapStyles.css")), # CSS DO MAPY !WAZNE
      fluidRow(
        titlePanel(title = "Analiza aktualnych danych dla Świata"),
        sidebarPanel(
          selectInput(
            inputId = "Kraj",
            label = "Wybierz kraj",
            choices = unique(coronavirus$Country.Region),
            selected = "Germany"
          )
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

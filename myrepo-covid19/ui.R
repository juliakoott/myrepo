# ! encoding: utf-8

library(shiny)              #installed 24.08.20
library(shinydashboard)     #installed 24.08.20

library(rvest)
#   ----    MAPY:  Biblioteki, Pliki, Funkcje ,Stale  ----
library(shinyjs)            # !!
library(leaflet)            #installed 24.08.20
library("tmap")             #installed 24.08.20
library(reshape2)
library(leaflet.providers)  #installed 24.08.20
library(shinyWidgets)
library(coronavirus)
library(leaflet)            #installed 24.08.20
library(COVID19)
library(plotly)
# !Musi byc tu : 
source("mapsFiles/polish-Covid19-data.R", encoding = "UTF-8") 
updateCovid19PL()
# updateCovid19PL() Musi byc na poczatku  w ui.R inaczej wystapi blad w
# sliderInput() dla daty dla map Polski poniewaz dane  wczytywane z
# updateCovid19PL() zawieraja maksymalna dostepna date dla danych polskich 

#   ----  END:  MAPY  Biblioteki, Pliki, Funkcje  ----
source('mapsFiles/covid-maps.R') 
source("Predykcja.r")
source("Polska_wykresy.r")
source("swiat_wykresy.r")
source("Polska_wykresy_proc.r") 
source("Swiat_podsumowanie.r")





sidebarUI <- dashboardSidebar(
	sidebarMenu(
		menuItem("Mapa", tabName = "Mapa", icon = icon("map-marked-alt")),
		menuItem("Polska", tabName = "Polska", icon = icon("chart-bar")),
		menuItem("Swiat", tabName = "Swiat", icon = icon("globe-africa")),
		menuItem("Predykcja", tabName = "Predykcja", icon = icon("chart-line")),
		menuItem("Informacje", tabName = "Informacje", icon = icon("thumbtack")),
		menuItem("Dane", tabName = "Dane", icon = icon("file-alt")),
		menuItem("Potwór spagetti", tabName = "Opis", icon = icon("pastafarianism"))
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
    tabItem(tabName = "Swiat",
    				fluidRow(infoBox("CHORZY", mch, icon = icon("head-side-virus"),color = "green"),infoBox("ZMARLI", mz, icon = icon("skull-crossbones"),color = "purple"), infoBox("WYZDROWIALI", mo, icon = icon("laugh-beam"))),
    				fluidRow(tags$h3(
    					box(pickerInput(
    						inputId = "Kraj",
    						label = 'Wybierz kraj',
    						choices=unique(covid19()[1])),width =100)),
    					tabBox(title = "Analiza aktualnych danych dla Świata", side = 'right',width=100,
    								 tabPanel("Analiza zachorowań",
    								 				 plotlyOutput('plotZ')),
    								 tabPanel("Analiza zgonów",
    								 				 plotlyOutput('plotD')),
    								 tabPanel('Analiza wyzdrowień',
    								 				 plotlyOutput('plotW')),
    								 tabPanel('Analiza ilocsi testów',
    								 				 plotlyOutput('plotT'))
    								 
    					)),
    				fluidRow(
    					tabBox(title = "Wykresy calosciowe", side='right',width=100,
    								 tabPanel("Analiza danych sumarycznych",
    								 				 p("Wybierz rodzaj danych: "),
    								 				 checkboxInput("confirmedSS", "Potwierdzone przypadki", FALSE),
    								 				 checkboxInput("deathSS", "Zmarli", FALSE),
    								 				 checkboxInput("recoveredSS", "Wyzdrowiali", FALSE),
    								 				 
    								 				 plotlyOutput('plotSS'),
    								 ),
    								 tabPanel("Analiza danych dziennych",
    								 				 p("Wybierz rodzaj danych: "),
    								 				 checkboxInput("confirmedDS", "Potwierdzone przypadki", FALSE),
    								 				 checkboxInput("deathDS", "Zmarli", FALSE),
    								 				 checkboxInput("recoveredDS", "Wyzdrowiali", FALSE),
    								 				 
    								 				 plotlyOutput('plotDS'))))
    ),
    
    # ===== POLSKA =====
    tabItem (tabName="Polska",
    				 fluidRow(infoBox("CHORZY", mchp, icon = icon("head-side-virus"),color = "green"),infoBox("ZMARLI", mzp, icon = icon("skull-crossbones"),color = "purple"), infoBox("WYZDROWIALI", mop, icon = icon("laugh-beam"))),
    				 
    				 fluidRow(
    				 	tabBox(title="Analiza aktualnych danych dla Polski", side="right",width=100,
    				 				 tabPanel("Analiza danych sumarycznych",
    				 				 				 p("Wybierz rodzaj danych: "),
    				 				 				 checkboxInput("confirmed", "Potwierdzone przypadki", FALSE),
    				 				 				 checkboxInput("death", "Zmarli", FALSE),
    				 				 				 checkboxInput("recovered", "Wyzdrowiali", FALSE),
    				 				 				 checkboxInput("testy", "Ilosci testów", FALSE),
    				 				 				 plotlyOutput('plotSUM'),
    				 				 ),
    				 				 tabPanel("Analiza danych dziennych",
    				 				 				 p("Wybierz rodzaj danych: "),
    				 				 				 checkboxInput("confirmedD", "Potwierdzone przypadki", FALSE),
    				 				 				 checkboxInput("deathD", "Zmarli", FALSE),
    				 				 				 checkboxInput("recoveredD", "Wyzdrowiali", FALSE),
    				 				 				 checkboxInput("testyD", "Ilosci testów", FALSE),
    				 				 				 plotlyOutput('plotDZIEN')
    				 				 ),
    				 				 
    				 				 tabPanel("Analiza procentowa",
    				 				 				 p("Wybierz rodzaj danych: "),
    				 				 				 checkboxInput("confirmedP", "Dzienny procent pozytywnych wyników", FALSE),
    				 				 				 checkboxInput("deathP", "Procent zmarych w Polsce", FALSE),
    				 				 				 checkboxInput("recoveredP", "Procent wyzdrowiałych w Polsce", FALSE),
    				 				 				 checkboxInput("testyP", "Procent pozytywnych testów w Polsce na przestrzeni dni", FALSE),
    				 				 				 plotlyOutput('plotPROC'))
    				 				 
    				 	)
    				 )
    ),
    
    
    # ===== PREDYKCJA PL =====
    tabItem(tabName = "Predykcja",
    				fluidRow(
    					box(tags$h3("Predykcja nowych przypadków w Polsce na podstawie aktualnych danych (model liniowy):"),
    							plotOutput('pred1'), width=100,
    							tags$h4('Numer dnia od początku pandemii'))
    				)
    ),
    # ===== INFORMACJE =====
    
    tabItem(tabName = "Informacje",
    				
    				fluidPage(
    					box(
    						tags$h3(icon('head-side-mask'),icon("viruses"),"Informacje:",
    										uiOutput("tab"),
    										uiOutput("tab2"),
    										uiOutput("tab3")), width=10),
    					box(imageOutput("IM1"), background = "purple", width=5, height=530),
    					box(imageOutput("IM2"), background = "purple", width=5,height=530)
    					
    				)
    ),
    
    
    # ===== RAPORT =====   
    
    tabItem(tabName="Dane",
    				numericInput("maxrows", "Pokaż wiersze:", 25),
    				verbatimTextOutput("rawtable"),
    				downloadButton("downloadCsv")),
    
    
    tabItem(tabName="Opis",
    				fluidRow(box(tags$div(
    					tags$h4("Zespół:"),
    					tags$br(),icon('check'),"Filipiak Szymon",
    					tags$br(),icon('check'),"Kądziołka Paulina",
    					tags$br(),icon('check'),"Kocikowska Olga",
    					tags$br(),icon('check'),"Kot Julia",
    					tags$br(),icon('check'),"Kula Marcin",
    					tags$br(),icon('check'),"Meihsner Adam",
    					tags$br(),icon('check'),"Taran Weronika",
    				),width=100)),
    				fluidRow(box(imageOutput("IM3"),width=100)
    				))
  
  )
)



ui <- dashboardPage(
	dashboardHeader(title = 'Covid-19'),
	skin = "purple",
	sidebarUI,
	dashboardUI
)



shinyUI(ui)

# ! encoding: utf-8

#  Sprawdzic ponizsze !!!!
# https://rstudio.github.io/shinydashboard/structure.html#sidebar

# Niemozna uzywac polskich znakow -nieaktualne trzeba ustawić kodowanie jako utf-8
# Demonstracja mapy w prostej aplikacji shiny 

# wczesniej wszystko bylo w projekcie wiec w w razie problemow z wczytaniem
# trzeba nadac biezacy katalog z setwd() lub otworzyc w projekcie
source('covid-maps.R')

library(shiny)
library(shinydashboard)
library(leaflet)

paragraphText = 'Powyzej mapa przedstawiająca aktualną liczbę  skumulowanych przypadków zachorowań na Covid-19'


sidebarUI <- dashboardSidebar(
    sidebarMenu(
        sliderInput("chosenDate",
                    "Wybierz datę:",
                    min = as.Date("2020-01-01","%Y-%m-%d"),
                    max = Sys.Date() - 1,
                    value = Sys.Date() - 1,
                    timeFormat="%Y-%m-%d")
        
    ), 
    collapsed = FALSE
)

dashboardUI = dashboardBody(
    leafletOutput("covMap", height = 800),
    p(paragraphText),
    p(textOutput(outputId = 'tekst1'))
)



ui <- dashboardPage(
    dashboardHeader(title = 'Mapa Covid-19'),
    sidebarUI,
    dashboardUI
)


server <- function(input, output, session) {
    
    output$tekst1 <- renderText({
        tekst1 = paste(as.character(input$chosenDate),
                       as.character(class(input$chosenDate))
                       )
    }
        
    )
    
    output$covMap <- renderLeaflet({
        initializeWorldMap()
    })
    
    
    renderDynamicMapElements = reactive({
        dynamicMapElements(input$chosenDate)
    })
    
    observe(
    {
        dynamicElements = renderDynamicMapElements()
        
        HTML_labels2map = dynamicElements$HTML_labels
        data2map_norm_radius = dynamicElements$data2map
        
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #            ---- USTAWIENIE PARAMETROW MAPY     ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # 
        coordCols = c('latitude','longitude')
        mapDesignOpts = list(color =  '#00b3b3',
                             weight = 1.2)
        
        # '#00b3b3'; # '#5e94ae' ciemno lazurowy 
        
        
        styleCSS = list(
            "color" = '#2f94b0',
            "box-shadow" = "3px 3px 5px 5px rgba(0,0,0,0.2)"
        )
        # "color" = '#2f94b0', "color" = '#5187a2', "color" = '#3988ae',
        
        labelOptionsList = labelOptions(textsize = "15px",style = styleCSS)
        
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #            ---- USTAWIENIE PARAMETROW MAPY     ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        
        
        
        # Dodanie kol do mapy
        leafletProxy(mapId = "covMap", data = data2map_norm_radius[,coordCols]) %>%
            clearMarkers() %>%
            addCircleMarkers(radius = data2map_norm_radius$confirmedRadius, 
                            weight = mapDesignOpts$weight, 
                                color = mapDesignOpts$color, 
                                label = HTML_labels2map, 
                                labelOptions = labelOptionsList)
        
        
    }

    )
    

}

shinyApp(ui, server)
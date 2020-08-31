# ! encoding: utf-8
# Demonstracja mapy w prostej aplikacji shiny 


library(shiny)              #installed 24.08.20
library(shinydashboard)     #installed 24.08.20


#   ----    MAPY    ----
# FUNKCJE ETC
source('mapsFiles/covid-maps.R') 

# BIBLIOTEKI
library(leaflet)            #installed 24.08.20
library("tmap")             #installed 24.08.20
library(leaflet.providers)  #installed 24.08.20

# https://shiny.rstudio.com/articles/scoping.html
# countries_pl.config = readRDS(file='mapsFiles/countryiesPl.rds') 
#   ----    MAPY    ----



server <- function(input, output, session) {
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       -----   COVID-19 MAPY   -----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$mapText_chosenDate <- renderText({
        mapText_chosenDate = paste('Wybrana data:', as.character(input$chosenDate))
    })
    
    output$covMap <- renderLeaflet({
        initializeWorldMap()
    })
    
    
    renderDynamicMapElements = reactive({
        dynamicElements = dynamicMapElements(input$chosenDate)
        
        if (input$chosenDataType == "kumulatywne"){
            dynamicElements$chosen_data2map = dynamicElements$data2map$confirmedRadius
            dynamicElements$mapDesignOpts$color = "#3399ff"
            
            output$mapText_info <- renderText({
                mapText_info = "Aktualna mapa obrazująca sumaryczną liczbę przypadków zachorowań na Covid-19"
            })
            
        }else if (input$chosenDataType == "śmierci"){
            const1 = 6
            dynamicElements$chosen_data2map = sqrt(const1*dynamicElements$data2map$deathsRadius^2)
            # dynamicElements$mapDesignOpts$color = "#9933ff"
            dynamicElements$mapDesignOpts$color = "#400080"
            
            output$mapText_info <- renderText({
                mapText_info = "Aktualna mapa obrazująca sumaryczną liczbę zgonów wywołanych Covid-19"
            })
            
            
        }else if (input$chosenDataType == "aktywne"){
            const1 = 6
            dynamicElements$chosen_data2map = sqrt(const1*dynamicElements$data2map$deathsRadius^2)
            dynamicElements$mapDesignOpts$color = "#00b3b3" # #009999
            
            output$mapText_info <- renderText({
                mapText_info = "Aktualna mapa obrazująca sumaryczną liczbę aktywnych przypadków"
            })
            
        }
        
        return(dynamicElements)
        
    })
    
    observe(
        {
            dynamicElements = renderDynamicMapElements()
            
            HTML_labels2map = dynamicElements$HTML_labels
            data2map_norm_radius = dynamicElements$data2map
            
            chosen_data2map = dynamicElements$chosen_data2map
            
            
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #            ---- USTAWIENIE PARAMETROW MAPY     ----
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            coordCols = c('latitude','longitude')
            mapDesignOpts = dynamicElements$mapDesignOpts
            
            styleCSS = list(
            #     # # "color" = 'rgb(34, 45, 50)',
            #     "color" = '#2f94b0',
            #     "box-shadow" = "1px 1px 5px rgba(0,0,0,0.5)",
            #     "background" = "rgba(255,255,255,0.8)",
            #     "border-radius" = "0px"
            )
            
            labelOptionsList = labelOptions(textsize = "1.4rem", style = styleCSS, className="mapLabel")
            
            
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #        ---- AKTUALIZACJA ELEMENTOW MAPY     ----
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            leafletProxy(mapId = "covMap", data = data2map_norm_radius[,coordCols]) %>%
                clearMarkers() %>%
                addCircleMarkers(
                    radius = chosen_data2map,       # Dodanie kol do mapy
                    weight = mapDesignOpts$weight,
                    color = mapDesignOpts$color,
                    label = HTML_labels2map,
                    labelOptions = labelOptionsList
                )
        }
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # COVID-19 MAPY :: KONIEC
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    
    
}

shinyServer(server)

# ! encoding: utf-8
# Demonstracja mapy w prostej aplikacji shiny


library(shiny)              #installed 24.08.20
library(shinydashboard)     #installed 24.08.20


#   ----    MAPY:  Biblioteki, Pliki, Funkcje ,Stale  ----
#   
### MAPY FUNKCJE ETC
# Argument okreslajacykodowanie UTF-8 jest wazny pon w kodzie sa polskie znaki
# kt sa nast uzyte w mapie
source('mapsFiles/covid-maps.R', encoding = "UTF-8")
source("mapsFiles/polish-Covid19-data.R", encoding = "UTF-8") 
# updateCovid19PL()
initializeConstantMapsObjects()
# styleCSS = list()

### MAPY BIBLIOTEKI
library(shinyjs)
library(leaflet)            #installed 24.08.20
library("tmap")             #installed 24.08.20
library(leaflet.providers)  #installed 24.08.20

# https://shiny.rstudio.com/articles/scoping.html
# countries_pl.config = readRDS(file='mapsFiles/countryiesPl.rds')
# 
#   ----  END:  MAPY  Biblioteki, Pliki, Funkcje  ----


server <- function(input, output, session) {
  
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# COVID-19 MAPY   ----
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 
  # POBRANIE DANYCH POLSKICH WOJEWODZTW CO 60 minut
  
  autoCheckUpdate = reactiveTimer(3600000)
  observe({
    autoCheckUpdate()
    cat('\n\n')
    print(paste("sprawdzam update!", as.character(Sys.time())))
    updateCovid19PL()
  })
  
  # Odsluga guzika urywajacego #on_map_box
  # https://stackoverflow.com/questions/44790028/show-hide-entire-box-element-in-r-shiny
  # Mapa Swiata
  observeEvent(input$mapWorld_showButton, {
    if (input$mapWorld_showButton %% 2 == 1) { #
      # print("shinyjs::hide")
      shinyjs::hide(id = "map_hideBox")
      
      # https://shiny.rstudio.com/reference/shiny/0.14/updateActionButton.html
      updateActionButton(session, "mapWorld_showButton",
                         label = "Pokaz")
    }else{
      # print("shinyjs::show")
      shinyjs::show(id = "map_hideBox")
      updateActionButton(session, "mapWorld_showButton",
                         label = "Ukryj")
    }
  })
  
  # Mapa Polski
  observeEvent(input$mapPL_showButton, {
    if (input$mapPL_showButton %% 2 == 1) { #
      
      shinyjs::hide(id = "map_PL_hideBox")
      updateActionButton(
        session, "mapPL_showButton", label = "Pokaz"
      )
    }else{
      
      shinyjs::show(id = "map_PL_hideBox")
      updateActionButton(
        session, "mapPL_showButton", label = "Ukryj"
      )
    }
  })
	
  # Elementy kt sa w #on_map_box
	output$mapText_chosenDate <- renderText({
		mapText_chosenDate = paste('Wybrana data:', as.character(input$chosenDate))
	})
	
	output$covMap <- renderLeaflet({
		initializeWorldMap()
	})
	
	
	renderDynamicMapElements = reactive({
		dynamicElements = dynamicMapElements(input$chosenDate)
		
		if (input$chosenDataType == "kumulatywne") {
			dynamicElements$chosen_data2map = dynamicElements$data2map$confirmedRadius
			dynamicElements$mapDesignOpts$color = "#3399ff"
			
			output$mapText_info <- renderText({
				mapText_info = "Aktualna mapa obrazująca sumaryczną liczbę przypadków zachorowań na Covid-19"
			})
			
		}else if (input$chosenDataType == "aktywne") {
			const1 = 1
			dynamicElements$chosen_data2map = sqrt(const1 * dynamicElements$data2map$activeRadius^2)
			dynamicElements$mapDesignOpts$color = "#00b3b3"
			
			output$mapText_info <- renderText({
				mapText_info = "Aktualna mapa obrazująca sumaryczną liczbę aktywnych przypadków"
			})
			
		}else if (input$chosenDataType == "zgony") {
			const1 = 5
			dynamicElements$chosen_data2map = sqrt(const1 * dynamicElements$data2map$deathsRadius ^
																						 	2)
			# dynamicElements$mapDesignOpts$color = "#9933ff" 
			dynamicElements$mapDesignOpts$color =  "#006699" # #003399 #8c00ff #cc0000 "#400080"
			
			output$mapText_info <- renderText({
				mapText_info = "Aktualna mapa obrazująca sumaryczną liczbę zgonów wywołanych Covid-19"
			})
		}
		
		return(dynamicElements)
		
	})
	
	observe({
		dynamicElements = renderDynamicMapElements()
		
		HTML_labels2map = dynamicElements$HTML_labels
		data2map_norm_radius = dynamicElements$data2map
		
		chosen_data2map = dynamicElements$chosen_data2map
		
		
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		#       ---- Ustawienie parametrow mapy     ----
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		
		coordCols = c('latitude', 'longitude')
		mapDesignOpts = dynamicElements$mapDesignOpts
		
		labelOptionsList = labelOptions(
			textsize = "1.4rem",
			# style = styleCSS,
			className = "mapLabel"
		)
		
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		#        ---- Aktualizacja elementow mapy    ----
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		
		leafletProxy(mapId = "covMap", data = data2map_norm_radius[, coordCols]) %>%
			clearMarkers() %>%
			addCircleMarkers(
				radius = chosen_data2map,
				# Dodanie kol do mapy
				weight = mapDesignOpts$weight,
				color = mapDesignOpts$color,
				label = HTML_labels2map,
				labelOptions = labelOptionsList
			)
	})

	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Covid-19 mapy :: POLSKA ----
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	output$mapText_chosenDate_PL <- renderText({
	  mapText_chosenDate_PL = paste('Wybrana data:', as.character(input$chosenDate))
	})
	
	output$covMap_PL <- renderLeaflet({
	  initializePolandMap()
	})
	
	
	# renderDynamicMapElements_PL = reactive({
	#   dynamicElements_PL = dynamicMapElements(input$chosenDate_PL, dataType = 'poland')
	#   
	#   if (input$chosenDataType_PL == "kumulatywne") {
	#     dynamicElements_PL$chosen_data2map = dynamicElements_PL$data2map$confirmedRadius
	#     dynamicElements_PL$mapDesignOpts$color = "#3399ff"
	#     
	#     output$mapText_info_PL <- renderText({
	#       mapText_info_PL = "Aktualna mapa obrazująca sumaryczną liczbę przypadków zachorowań na Covid-19"
	#     })
	#     
	#   }else if (input$chosenDataType_PL == "zgony") {
	#     const1 = 5
	#     dynamicElements_PL$chosen_data2map = sqrt(const1 * dynamicElements_PL$data2map$deathsRadius^2)
	#     # dynamicElements_PL$mapDesignOpts$color = "#9933ff" #ff0066
	#     dynamicElements_PL$mapDesignOpts$color = "#400080"
	#     
	#     output$mapText_info_PL <- renderText({
	#       mapText_info_PL = "Aktualna mapa obrazująca sumaryczną liczbę zgonów wywołanych Covid-19"
	#     })
	#   }
	#   
	#   return(dynamicElements_PL)
	#   
	# })
	output$mapText_info_PL <- renderText({
    mapText_info_PL = "Aktualna mapa obrazująca sumaryczną liczbę przypadków zachorowań na Covid-19"
  })
	
	observe({
	  dynamicElements_PL = dynamicMapElements(input$chosenDate_PL, dataType = 'poland')
	  dynamicElements_PL$chosen_data2map = dynamicElements_PL$data2map$confirmedRadius
	  # dynamicElements_PL = renderDynamicMapElements_PL()
	  
	  HTML_labels2map_PL = dynamicElements_PL$HTML_labels
	  data2map_norm_radius_PL = (dynamicElements_PL$data2map)
	  chosen_data2map_PL = dynamicElements_PL$chosen_data2map
	  dynamicElements_PL$mapDesignOpts$color = "#3399ff"
	  
	  
	  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	  #            ---- Ustawienie parametrow mapy     ----
	  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	  
	  # coordCols = c('latitude', 'longitude')
	  mapDesignOpts_PL = dynamicElements_PL$mapDesignOpts
	  
		# styleCSS_PL = list()
	  labelOptionsList_PL = labelOptions(
      textsize = "1.4rem",
      # style = styleCSS_PL,
      className = "mapLabel"
    )
	  coordCols =  c('latitude', 'longitude')
	  
	  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	  #        ---- Aktualizacja elementow mapy     ----
	  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	  leafletProxy(mapId = "covMap_PL", data = data2map_norm_radius_PL[, coordCols]) %>%
	    clearShapes() %>%
	    addPolygons(
	      data = .map_polishPolygons,
	      fillColor = ~.map_Palette(data2map_norm_radius_PL$deaths),  fillOpacity = 0.2, 
	      color = "#000000", weight = 1.2, opacity = 0.2,  smoothFactor = 0 #3399ff
	    ) %>%  clearControls() %>%
	    addLegend(
	      pal = .map_Palette, values = data2map_norm_radius_PL$deaths, 
	      opacity = 0.5, title = "Zgony na 1mln mieszkańców", position = "topleft"
      ) %>%
	    clearMarkers() %>%
	    addCircleMarkers(
	      radius = chosen_data2map_PL,
	      # Dodanie kol do mapy
	      weight = mapDesignOpts_PL$weight,
	      color = mapDesignOpts_PL$color,
	      label = HTML_labels2map_PL,
	      labelOptions = labelOptionsList_PL
	    ) 
	})
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# COVID-19 MAPY :: Koniec ----
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	
}

shinyServer(server)

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


# BIBLIOTEKI
library(leaflet) #installed 24.08.20
library(tmap)             #installed 24.08.20
library(leaflet.providers)  #installed 24.08.20
library(ggplot2)
library(shinyjs)
library(plotly)
library(COVID19)
d<-covid19()



source("Predykcja.r")
source("Polska_wykresy.r")
source("swiat_wykresy.r")
source("Polska_wykresy_proc.r") 
source("Swiat_podsumowanie.r")

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

	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#       -----  SWIAT   -----
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	#SUMARYCZNY
	output$plotD<-renderPlotly(
		
		figzs <- plot_ly(
			type = "scatter",
			mode = "lines",
			line = list(
				color = '#008000'),
			x = as.Date(m$date, format= "%Y-%m-%d"),
			y = d[which(d$id==input$Kraj),]$deaths)
	)
	
	output$plotZ<-renderPlotly(
		figcs <- plot_ly(
			type = "scatter",
			mode = "lines",
			line = list(
				color = '#dc143c'),
			x = as.Date(m$date, format= "%Y-%m-%d"), 
			y = d[which(d$id==input$Kraj),]$confirmed)
	)
	
	output$plotW<-renderPlotly(
		figos <- plot_ly(
			type = "scatter",
			mode = "lines",
			line = list(
				color = '#800000'),
			x = as.Date(m$date, format= "%Y-%m-%d"), 
			y =  d[which(d$id==input$Kraj),]$recovered)
	)
	
	output$plotT<-renderPlotly(
		figts <- plot_ly(
			type = "scatter",
			mode = "lines",
			line = list(
				color = '#800080'),
			x = as.Date(m$date, format= "%Y-%m-%d"), 
			y = d[which(d$id==input$Kraj),]$tests)
	)
	
	
	#### WYKRESY SUMARYCZNE SWIAT
	output$plotSS<-renderPlotly(
		if(input$deathSS==TRUE)
			return(figzss <- figzss %>% layout(
				title = " Ogólna liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba zgonów')))
		
		else if(input$confirmedSS==TRUE)
			return(figcss <- figcss %>% layout(
				title = " Ogólna liczba zdiagnozowanych przypadków choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba chorych')))
		
		else if(input$recoveredSS==TRUE)
			return(figoss <- figoss %>% layout(
				title = " Ogólna liczba osób wyzdrowiaylch z  choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba ozdrowienców')))
		
		
	)
	
	#### WYKRESY DZIENNE SWIAT    
	output$plotDS<-renderPlotly(
		if(input$deathDS==TRUE)
			return(figzss1 <- figzss1 %>% layout(
				title = "Liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba zgonów')))
		
		else if(input$confirmedDS==TRUE)
			return(figcss1 <- figcss1 %>% layout(
				title = " Liczba zdiagnozowanych przypadków choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba chorych')))
		
		else if(input$recoveredDS==TRUE)
			return(figoss1 <- figoss1 %>% layout(
				title = "Liczba osób wyzdrowialych z  choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba ozdrowienców')))
	)
	
	
	
	
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#       -----  POLSKA   -----
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	#SUMARYCZNY
	output$plotSUM<-renderPlotly(
		if(input$death==TRUE)
			return(figz <- figz %>% layout(
				title = " Ogólna liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba zgonów')))
		
		else if(input$confirmed==TRUE)
			return(figc <- figc %>% layout(
				title = " Ogólna liczba zdiagnozowanych przypadków choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba chorych')))
		
		else if(input$recovered==TRUE)
			return(figo <- figo %>% layout(
				title = " Ogólna liczba osób wyzdrowialych z  choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba ozdrowieńców')))
		
		else if(input$testy==TRUE)
			return(figt <- figt %>% layout(
				title = " Ogólna Liczba testów przeprowadzonych w celu wykrycia choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba testów')))
	)
	
	#DZIENNY
	output$plotDZIEN<-renderPlotly(
		if(input$deathD==TRUE)
			return(figz1 <- figz1 %>% layout(
				title = "Liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba zgonów')))
		
		else if(input$confirmedD==TRUE)
			return(figc1 <- figc1 %>% layout(
				title = "Liczba wykrytych przypadków choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba chorych')))
		
		else if(input$recoveredD==TRUE)
			return(figo1 <- figo1 %>% layout(
				title = "Liczba wyzdrowialych osób z choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba ozdrowieńców')))
		
		else if(input$testyD==TRUE)
			return(figt1 <- figt1 %>% layout(
				title = "Liczba testów przeprowadzonych w celu wykrycia choroby covid19 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Liczba testów')))
	)
	
	# %  
	output$plotPROC<-renderPlotly(
		if (input$deathP==TRUE)
			return( fig2 <- fig2 %>% layout(
				title = "Procent zgonów wywolanych przez chorobe covid19 w Polsce w porówaniu ze zgonami na świecie na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Procent wyników [%]')))
		else if(input$confirmedP==TRUE)
			return( fig4 <- fig4 %>% layout(
				title = "Procent  zdiagnozowanych przypadków choroby covid19 w Polsce w porówaniu z przypadkami na świecie na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Procent wyników [%]')))
		else if(input$recoveredP==TRUE)
			return( fig3 <- fig3 %>% layout(
				title = "Procent wyzdrowień z choroby covid19 w Polsce w porówaniu z wyzdrowieniami na świecie na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Procent wyników [%]')))
		else if(input$testyP)
			return(fig1 <- fig1 %>% layout(
				title = "Procent pozytwnych wyników na obecnosć wirusa SARS-COV-2 na przestrzeni dni",
				xaxis = list(type = 'date',title = "Data"),
				yaxis = list(title = 'Procent wyników [%]')))
	)
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#       -----  PREDYKCJA   -----
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	output$pred1 <- renderPlot({
		ggplot(data = covid_PL, aes(x = date, y = confirmed)) +
			geom_point() +
			stat_smooth(method = "lm", col = "dodgerblue3") +
			theme(panel.background = element_rect(fill = "white"),
						axis.line.x=element_line(),
						axis.line.y=element_line()) #+
		#ggtitle("Linear Model Fitted to CovidData")  
	})
	
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#       -----  INFORMACJE   -----
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	#URL
	url <- a("World Health Organization", href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019?gclid=CjwKCAjwlZf3BRABEiwA8Q0qqyA0AT6PqeRz4nxMeCfOv0GU-CD7HOlMNSaVakfqywOzM6bzIrmEohoCuLoQAvD_BwE")
	url2 <- a("Aktualne informacje rządowe", href="https://www.gov.pl/web/koronawirus")
	url3 <- a("Główny Inspektorat Sanitarny", href="https://gis.gov.pl/")
	
	output$tab <- renderUI({
		tagList("WHO link:", url)})
	output$tab2 <- renderUI({
		tagList("GOV link:", url2)})
	output$tab3 <- renderUI({
		tagList("GIS link:", url3)})
	#obrazki   
	output$IM1 <- renderImage({
		pfad <- "https://gis.gov.pl/wp-content/uploads/2018/04/mycie-r%C4%85k-kwadrat.png"
		list(src="rece.png",
				 contentType = 'image/png',
				 width = 400,
				 height = 500,
				 alt = "gis")
	}, deleteFile = F)
	output$IM2 <- renderImage({
		
		list(src="zalecenia.png",
				 contentType = 'image/png',
				 width = 400,
				 height = 500,
				 alt = "zalec")
	}, deleteFile = F)
	
	output$IM3 <- renderImage({
		
		list(src="somsiad.png",
				 contentType = 'image/png',
				 #width = 400,
				 #height = 500,
				 alt = "mem")
	}, deleteFile = F)
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#       -----  RAPORT   -----
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
	output$downloadCsv <- downloadHandler(
		filename = 'raport',
		content = function(file) {
			write.csv(d, file)
		}
	)
	
	output$rawtable <- renderPrint({
		options(width = 1000)
		print(tail(d, input$maxrows), row.names = FALSE)
		
	})
	
	
}

shinyServer(server)

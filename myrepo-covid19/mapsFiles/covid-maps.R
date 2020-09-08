# ! encoding: utf-8

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           ---- WCZYTANIE PAKEIETOW  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Pakiety dane covid-19

if (!require('COVID19')) {
    install.packages('COVID19')  #installed 24.08.20
    library('COVID19')
}
library('COVID19')
print("wczytano COVID19")

# ~~~~~~~~~~~~~~~~~~~~~~~~
# Pakiety dotyczace map
# ........................

if (!require('leaflet')) {
    install.packages('leaflet')      #installed 24.08.20
    library("leaflet")
}


# Inne pakiety

if (!require('tibble')) {
    install.packages('tibble')    #installed 24.08.20
    library("tibble")
}


library("magrittr")
# DODATKI
library("tmap")
library(leaflet.providers)
library("V8") # do leaflet.providers



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       ----   MAPY FUNKCJE NISKOPOZIOMOWE  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


addActiveCases <- function(DataFrame2map) {
    # Używane w funkcji dynamicMapElements()
    # 
    # Funkcja dodaje aktywne przypadki na podstawie kolumn
    # confirmed, recovered i deaths
    # 
    # TODO:
    # dodać sprawdzanie poprawniosci wejściowej ramki danych!
    
    active = DataFrame2map$confirmed - DataFrame2map$recovered - DataFrame2map$deaths
    DataFrame2map = add_column(DataFrame2map, active,.after = 'recovered')
    
    return(DataFrame2map)
}



computeRadius <- function(
   data2map_norm, radius1000 = 500, 
   cols2radius = c('tests', 'confirmed', 'recovered', 'active', 'deaths')
)
{
    # Używane w funkcji dynamicMapElements()
    # 
    # data2map_norm - dane znormalizowane na 1000 obywateli
    # radius1000 - promien w stytuacji kiedy liczba przypadkow wynioslaby 1000
    # na 1000 obywateli 
    
    radiusFactor = radius1000 / sqrt(10^6) # 10^6 stala na ile populacji jest normalizowane
    
    data2map_norm_radius = data2map_norm
    data2map_norm_radius[ ,cols2radius] = sqrt(data2map_norm[ ,cols2radius])*radiusFactor
    
    whereCols = colnames(data2map_norm_radius) %in% cols2radius
    newColnames = paste0(colnames(data2map_norm_radius)[whereCols],'Radius')
    colnames(data2map_norm_radius)[whereCols] = newColnames
    
    return(data2map_norm_radius)
}

readPolishPolygons <- function() {
  polishPolygons = readRDS(file = "mapsFiles/poland_geojson.rds")
  polishPolygons@polygons
  assign(x = ".map_polishPolygons",  value = polishPolygons, envir = .GlobalEnv)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       ----   MAPY FUNKCJE WYSOKOPOZIOMOWE  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

initializeConstantMapsObjects <- function() {
  ## Stworzenie stalych obiektow takich jak poligony, palety, etc
  readPolishPolygons()
  
  ## Stworzenie palety
  ## paleta pomarancze:"#ffffcc","#ffcc99","#ff9966","#ff6600","#990033"
  ## paleta fiolety:"#ccccff","#9999ff","#9966ff","#6600ff","#6600cc"
  # #ffffcc #ffcc00 #ff9933 #ff3300 #990000
  # c("#ccccff","#9999ff","#9966ff","#6600ff","#6600cc")
	map_Palette = colorBin(
		# c("#ffffff","#e6e6ff", "#ccb3ff", "#6600ff","#3333cc", "#191966","#0f0f3d"),
		c("#ffffff","#e6fcff", "#99f3ff", "#4deaff","#00cbe6", "#008799","#002d33"),
		# c("#ffffff","#e6f7ff", "#99ddff", "#4dc3ff","#0099e6", "#006699","#00334d"),
		domain = NULL, bins = c(.0, 10, 25.0, 50.0, 100.0, 250.0, 1000.0, 10^6)
	)
	
  assign(".map_Palette", value = map_Palette, envir = .GlobalEnv)
} 



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       ----   STWORZENIE MAPY covid-19  ----
# 
# Dane z pakietu COVID19
# wersja dla 
# + skumulowanej liczby przypadkow
# + liczby aktywnych przypadkow
# + liczby zgonow
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#            ---- INICJALIZACJA MAPY LEAFLET     ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
initializeWorldMap = function(){
    
    # Stworzenie obiektu leaflet i ustawienie opcji minimalnego i maksymalnego powiększenia
    map <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 7))
    
    # Ustawienie widoku początkowego mapy:
    map <- setView(map, lng = 25, lat = 48, zoom = 4) %>%
        setMaxBounds(lng1 = -240, lng2 = 280, lat1 = -180, lat2 = 180)  %>%
        addProviderTiles(provider = 'CartoDB.Voyager') # Dodanie dostawcy mapy innego niz domyslny
        # addProviderTiles(provider = providers$CartoDB.Voyager) # Dodanie dostawcy mapy innego niz domyslny
    
    # proba dodania dodatkowych dostawcow map:
    # leaflet.providers::get_providers()
    
    # tmap_mode("view")
    # qtm()
    # Dodanie dostawcy mapy
    # http://leaflet-extras.github.io/leaflet-providers/preview/index.html

    return(map)
}

# Mapa Polski
initializePolandMap = function(){
    
    # Stworzenie obiektu leaflet i ustawienie opcji minimalnego i maksymalnego powiększenia
    map <- leaflet()
      # options = leafletOptions(minZoom = 6, maxZoom = 8)
    
    # Ustawienie widoku początkowego mapy: (22.5, 53)
    map <- setView(map, lng = 19.5, lat = 52, zoom = 7, options = leafletOptions(minZoom = 6, maxZoom = 8)) %>%
      setMaxBounds(lng1 = 8, lng2 = 32, lat1 = 42, lat2 = 56)  %>%
      addProviderTiles(provider = 'CartoDB.Voyager') # %>% # Dodanie dostawcy mapy innego niz domyslny
      

    return(map)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           ----  ELEMENTY DYNAMICZNE MAPY     ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dynamicMapElements <- function(chosendate, dataType="world") {
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #       ----      WD :: EKSTRAKCJA DANYCH covid-19  ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (dataType=="world") {
    
  
    covnat.chosen = covid19(start = chosendate, end = chosendate, cache = TRUE)
    
    # Wyczyszczenie Danych
    covnat.chosen = covnat.chosen[!is.na(covnat.chosen$latitude), ]
    covnat.chosen = covnat.chosen[!is.na(covnat.chosen$confirmed), ]
    
    covnat.chosen = covnat.chosen[sample(1:nrow(covnat.chosen), size=nrow(covnat.chosen), replace = F),]
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #           ---- WD :: Poprawki Danych Do Mapy ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # 1. Poprawienie koordynat Danii
    toImprove_idx = which(covnat.chosen$iso_alpha_3 == "DNK")
    covnat.chosen[toImprove_idx, c('latitude', 'longitude')] = t(as.matrix(c(55.6, 10.32)))
    
    # 2. POPRAWIENIE NAZW PANSTW NA POLSKIE
    # !z pliku countriesPl.rds została usunięta Palestyna i statki wycieczkowe (tez sa w danych)
    # z powodu braku wspolrzednych geograficznych!
    
    # Wczytanie pliku konfiguracyjnego z polskimi nazwami panst
    # poki co nazwy z angielskich sa zmieniane na polskie przy kazdyej 
    if (!exists("countries_pl.config", envir = .GlobalEnv)) {
        countries_pl.config = readRDS(file = 'mapsFiles/countriesPl.rds') # 
        assign('countries_pl.config', countries_pl.config, envir = .GlobalEnv)
        
    } else{
        get('countries_pl.config', envir = .GlobalEnv)
    } 
    
    # Posortowanie wierszy wg kodu iso alpha 3 panstw rosnaco
    if (is.unsorted(covnat.chosen$iso_alpha_3)) {
        idxs = order(covnat.chosen$iso_alpha_3)
        covnat.chosen = covnat.chosen[idxs, ]
    }
    
    # Jesli liczba
    is_countryies_PL = F
    if (length(covnat.chosen$iso_alpha_3) == length(countries_pl.config$ISO3)) {
        if (all(covnat.chosen$iso_alpha_3 == countries_pl.config$ISO3)) {
            covnat.chosen$administrative_area_level_1 = countries_pl.config$PANSTWO
            is_countryies_PL = T
        }
    }
  
    if (!is_countryies_PL) {
        warning("nieudana proba zmiany nazw panstw na polskie!")
        # Trzebaby dodac dopasowanie kodow iso3 pom danymi i ramka dnych z pliku
        # konfiguracyjnego za pomoca match, tak zeby jesli z jakichs powodow w
        # danych z pakietu COVID19 zmieni sie liczba panstw dalo sie nadal zmienic
        # nazwy na polskie
    }
  
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       DODANIE KOLUMNY AKTYWNYCH PRZYPADKOW
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data2map = addActiveCases(covnat.chosen)
    data2map[data2map$active < 0, "active"] = 0
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       NORMALIZACJA WZGLEDEM POLULACJI na 1mln
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cols2norm = c('tests', 'confirmed', 'recovered', 'active', 'deaths')  # KOLUMNY DLA SWIATA
    data2map_norm = data2map
    data2map_norm[, cols2norm] = (data2map[, cols2norm] / covnat.chosen$population) * 10^6 
    # data2map$population = covnat.chosen$population  # TODO: POPRAWIC
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #        ----  Przeskalowanie wartosci  na promien  ----
    # (pole powierzchnii proporcjonalne do liczby przypadkow)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # cols2radius = c('tests', 'confirmed', 'recovered', 'active', 'deaths')
    
    data2map_norm_radius = computeRadius(data2map_norm, radius1000 = 250)
    # data2map_norm_radius nie ma juz wartosci 
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #            ----  Stworzenie etykiet     ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Etykiety sa elementem dynamicznym zależnym od chosendate
    
    # formatStr = '<b>%s</b> <div>Confirmed na 1mln: %.1f<br/>Active na 1mln: %.1f<br/>Deaths na 1mln: %.1f <br/><span id="labPopul">Population %.3f mln</span></div>'
    formatStr = '<b>%s</b> <div>Sumarycznie na 1mln: %.1f<br/>Aktywne na 1mln: %.1f<br/>Zgony na 1mln: %.1f <br/><span id="labPopul">Populacja %.3f mln</span></div>'
    
    labels2map = sprintf(
      fmt = formatStr,
      data2map_norm$administrative_area_level_1,
      data2map_norm$confirmed,
      data2map_norm$active,
      data2map_norm$deaths,
      round((data2map$population/10^6),3)
    )
    
    # Przekszatalcenie etykiet na klase HTML (oznaczenie tekstu jako HTML)
    HTML_labels2map = lapply(X = labels2map, htmltools::HTML)
    
    dynamicElements = list()
    dynamicElements$HTML_labels = HTML_labels2map
    dynamicElements$data2map = data2map_norm_radius
    dynamicElements$mapDesignOpts = list(
      color =  '#00b3b3',  weight = 1.2
    )
    
  }else if (dataType == "poland") {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                   ---- POLSKA ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # TODO: DODAĆ DANE POLSKIE
    # covnat.chosen = covid19PL(chosen.date)
    covnat.chosen = covid19PL(chosendate)
    
    ## TODO: posprzatac tutaj
    idx123 = !(covnat.chosen$voivodeship %in% c("dzienne", "cumulatywnie"))
    covnat.chosen = covnat.chosen[idx123, ]
    
    data2map = covnat.chosen # TODO: do podmienienia !
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       NORMALIZACJA WZGLEDEM POLULACJI na 1mln
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cols2norm = c('confirmed', 'deaths')  # KOLUMNY DLA POLSKI
    
    data2map_norm = data2map
    data2map_norm[, cols2norm] = (data2map[, cols2norm] / covnat.chosen$population) * 10^6 
    data2map$population = covnat.chosen$population
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #        ----  Przeskalowanie wartosci  na promien  ----
    # (pole powierzchnii proporcjonalne do liczby przypadkow)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data2map_norm_radius = computeRadius(data2map_norm, radius1000 = 1000, cols2radius = c('confirmed'))
    # data2map_norm_radius nie zaiera tutaj juz column deaths i  confirmed tylko  deathsRadius i  confirmed (sic!)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #            ----  Stworzenie etykiet     ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Etykiety sa elementem dynamicznym zależnym od chosendate
    
    # formatStr = '<b>%s</b> <div>Confirmed na 1mln: %.1f<br/>Active na 1mln: %.1f<br/>Deaths na 1mln: %.1f <br/><span id="labPopul">Population %.3f mln</span></div>'
    formatStr = '<b>%s</b> <div>Przypadki: %d<br/>Zgony: %d<br/>Sumarycznie na 1mln: %.1f<br/>Zgony na 1mln: %.1f <br/><span id="labPopul">Populacja %.1f mln</span></div>'
    
    labels2map = sprintf(
      fmt = formatStr,
      data2map_norm$voivodeship,
      data2map$confirmed,
      data2map$deaths,
      data2map_norm$confirmed,
      data2map_norm$deaths,
      round((data2map$population/10^6),3) # TODO: ZOBACZ TO RANO
    )
    
    # Przekszatalcenie etykiet na klase HTML (oznaczenie tekstu jako HTML)
    HTML_labels2map = lapply(X = labels2map, htmltools::HTML)
    
    dynamicElements = list()
    dynamicElements$HTML_labels = HTML_labels2map
    dynamicElements$data2map = data2map_norm_radius
    dynamicElements$mapDesignOpts = list(
      color =  '#00b3b3',  weight = 1.2
    )
  }
  
  return(dynamicElements)
}

# LITERATURA
# https://rstudio.github.io/leaflet/shiny.html

# # ****************************************************
# #                 ----    KONIEC     ---- 
# #             Ponizsza czesc mozna usunac
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # ****************************************************
# #            ---- CZESC DYNAMICZNA MAPY     ----
# #
# Funkcję dynamicMapPart() usunięto poniewaz aktualizacja czesci
# dynamicznych/zmiennych mapy nie moze odbywac sie w funkcji musi 
# odbywac sie bezposrednio w aplikacji
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# 
# dynamicMapPart = function(chosendate){
#     # UWAGA FUNKCJA AKTUALNIE NIE JEST UŻYWANA!!
#     
#     dynamicElements = dynamicMapElements(chosendate)
#     
#     HTML_labels2map = dynamicElements$HTML_labels
#     data2map_norm_radius = dynamicElements$data2map
#     
#     
#     
#     chosen_map_data = data2map_norm_radius$confirmedRadius
#     
#     
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     #            ---- USTAWIENIE PARAMETROW MAPY     ----
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     
#     # 
#     coordCols = c('latitude','longitude')
#     mapDesignOpts = list(color =  '#00b3b3',
#                          weight = 1.2)
#     
#     # '#00b3b3'; # '#5e94ae' ciemno lazurowy 
#     
#     
#     styleCSS = list(
#         "color" = '#2f94b0',
#         "box-shadow" = "3px 3px 5px 5px rgba(0,0,0,0.2)"
#     )
#     # "color" = '#2f94b0', "color" = '#5187a2', "color" = '#3988ae',
#     
#     labelOptionsList = labelOptions(textsize = "15px",style = styleCSS)
#     
#     
#     
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     #            ---- USTAWIENIE PARAMETROW MAPY     ----
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     
#     # Dodanie kol do mapy
#     map = leafletProxy(mapId = "covMap", data = data2map_norm_radius[,coordCols]) %>%
#         clearMarkers() %>%
#         addCircleMarkers(
#             radius = chosen_map_data, 
#             weight = mapDesignOpts$weight, 
#             color = mapDesignOpts$color, 
#             label = HTML_labels2map, 
#             labelOptions = labelOptionsList
#         )
# 
#     return(map)    
# }






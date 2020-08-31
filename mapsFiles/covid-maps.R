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
#       ----   MAPY FUNKCJE NIZSZYCH STOPNI  ----
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



computeRadius <- function(data2map_norm, radius1000 = 500,
                          cols2radius = c('tests', 'confirmed',
                                          'recovered', 'active', 'deaths')
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



dynamicMapElements <- function(chosendate) {
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       ----   EKSTRAKCJA DANYCH covid-19  ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    covnat.chosen = covid19(start = chosendate, end = chosendate, cache = TRUE)
    
    # Wyczyszczenie Danych
    covnat.chosen = covnat.chosen[!is.na(covnat.chosen$latitude), ]
    covnat.chosen = covnat.chosen[!is.na(covnat.chosen$confirmed), ]
    
    covnat.chosen = covnat.chosen[sample(1:nrow(covnat.chosen), size=nrow(covnat.chosen), replace = F),]
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #           ---- Poprawki Danych Do Mapy ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # 1. Poprawienie koordynat Danii
    toImprove_idx = which(covnat.chosen$iso_alpha_3 == "DNK")
    covnat.chosen[toImprove_idx, c('latitude', 'longitude')] = t(as.matrix(c(55.6, 10.32)))
    
    # 2. POPRAWIENIE NAZW PANSTW NA POLSKIE
    # !z pliku countryiesPl.rds została usunięta Palestyna i statki wycieczkowe (tez sa w danych)
    # z powodu braku wspolrzednych geograficznych!
    
    # Wczytanie pliku konfiguracyjnego z polskimi nazwami panst
    # poki co nazwy z angielskich sa zmieniane na polskie przy kazdyej 
    if (!exists("countries_pl.config", envir = .GlobalEnv)) {
        countries_pl.config = readRDS(file='mapsFiles/countryiesPl.rds')
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
    
    cols2norm = c('tests', 'confirmed', 'recovered', 'active', 'deaths')
    
    data2map_norm = data2map
    data2map_norm[, cols2norm] = (data2map[, cols2norm] / covnat.chosen$population) * 10^6 
    data2map$population = covnat.chosen$population
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #          Przeskalowanie na wartosci promienia
    # (pole powierzchnii proporcjonalne do liczby przypadkow)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # cols2radius = c('tests', 'confirmed', 'recovered', 'active', 'deaths')
    
    data2map_norm_radius = computeRadius(data2map_norm, radius1000 = 250)
    # data2map_norm_radius$confirmedRadius[data2map_norm_radius$id == 'USA']
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #            ----  STWORZENIE ETYKIET     ----
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






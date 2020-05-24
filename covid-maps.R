

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           ---- WCZYTANIE PAKEIETÓW  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Pakiety dane covid-19

if (!require('COVID19')) {
    install.packages('COVID19')
    library('COVID19')
}

# ~~~~~~~~~~~~~~~~~~~~~~~~
# Pakiety dotyczace map
# ........................

if (!require('leaflet')) {
    install.packages('leaflet')
    library("leaflet")
}


# Inne pakiety

if (!require('tibble')) {
    install.packages('tibble')
    library("tibble")
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       ----   MAPY FUNKCJE NIZSZYCH STOPNI  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


addActiveCases <- function(DataFrame2map) {
    # U¿ywane w funkcji dynamicMapElements()
    # 
    # Funkcja dodaje aktywne przypadki na podstawie kolumn
    # confirmed, recovered i deaths
    # 
    # TODO:
    # dodaæ sprawdzanie poprawniosci wejœciowej ramki danych!
    
    active = DataFrame2map$confirmed - DataFrame2map$recovered - DataFrame2map$deaths
    DataFrame2map = add_column(DataFrame2map, active,.after = 'recovered')
    
    return(DataFrame2map)
}



computeRadius <- function(data2map_norm, radius1000 = 500,
                          cols2radius = c('tests', 'confirmed',
                                          'recovered', 'active', 'deaths')
)
{
    # U¿ywane w funkcji dynamicMapElements()
    # 
    # data2map_norm - dane znormalizowane na 1000 obywateli
    # radius1000 - promien w stytuacji kiedy liczba przypadków wynioslaby 1000
    # na 1000 obywateli 
    
    radiusFactor = radius1000 / sqrt(10^3) # stala 
    
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
# + skumulowanej liczby przypadków
# + liczby aktywnych przypadków
# + liczby zgonow
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


initializeWorldMap = function(){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #            ---- INICJALIZACJA MAPY LEAFLET     ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Stworzenie obiektu leaflet i ustawienie opcji minimalnego i maksymalnego powiêkszenia
    map <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 7))
    
    # Ustawienie widoku pocz¹tkowego mapy:
    map <- setView(map, lng = 25, lat = 48, zoom = 5) %>%
        setMaxBounds(lng1 = -240, lng2 = 280, lat1 = -180, lat2 = 180)
    
    # Dodanie dostawcy mapy
    map <- addProviderTiles(map, provider = providers$CartoDB.Voyager)
    return(map)
}



dynamicMapElements <- function(chosendate) {
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       ----   EKSTRAKCJA DANYCH covid-19  ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # chosendate = 'currentdate'
    
    # if (chosendate == 'currentdate'){
    #     chosendate = Sys.Date() - 1
    # }
    covnat.chosen = covid19(start = chosendate, end = chosendate, cache = TRUE)
    
    # requireColumns = c('date', 'population', 'tests', 'confirmed', 'recovered', 'deaths',
    #                    'latitude','longitude')
    
    # Wyczyszczenie Danych
    covnat.chosen = covnat.chosen[!is.na(covnat.chosen$latitude), ]
    covnat.chosen = covnat.chosen[!is.na(covnat.chosen$confirmed), ]
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       DODANIE KOLUMNY AKTYWNYCH PRZYPADKOW
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ### POTRZEBNA DEFINICJA FUNKCJI
    # addActiveCases
    data2map = addActiveCases(covnat.chosen)
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       NORMALIZACJA NA 1000 OBYWATELI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    cols2norm = c('tests', 'confirmed', 'recovered', 'active', 'deaths')
    
    data2map_norm = data2map
    data2map_norm[, cols2norm] = (data2map[, cols2norm] / covnat.chosen$population) * 10^3 
    data2map$population = covnat.chosen$population
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #          Przeskalowanie na wartosci promienia
    # pole powierzchnii proporcjonalne do liczby przypadkow
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    radius1000 = 500 # promien odpowaidajacy 1000 przypadkow na 1000 obywateli
    radiusFactor = radius1000 / sqrt(10^3) # promien odpowaidajacy 10^6 przypadkow
    
    # radiusFactor = radius100k * sqrt(10^3/10^6) # promien odpowaidajacy 10^6 przypadkow
    
    cols2radius = c('tests', 'confirmed', 'recovered', 'active', 'deaths')
    
    ### POTRZEBNA DEFINICJA FUNKCJI
    # computeRadius
    
    
    data2map_norm_radius = computeRadius(data2map_norm, radius1000 = 250)
    data2map_norm_radius$confirmedRadius[data2map_norm_radius$id == 'USA']
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #            ----  STWORZENIE ETYKIET     ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Etykiety sa elementem dynamicznym
    
    formatStr = '<b>%s</b> <br/>Confirmed per 1000: %.3f <br/>Active per 1000: %.3f <br/>Deaths per 1000: %.3f'
    
    labels2map = sprintf(
        fmt = formatStr,
        data2map_norm$administrative_area_level_1,
        data2map_norm$confirmed,
        data2map_norm$active,
        data2map_norm$deaths
    )
    
    # Przekszata³cenie etykiet na klasê HTML (oznaczenie tekstu jako HTML)
    HTML_labels2map = lapply(X = labels2map, htmltools::HTML)
    
    dynamicElements = list()
    dynamicElements$HTML_labels = HTML_labels2map
    dynamicElements$data2map = data2map_norm_radius
    
    
    return(dynamicElements)
    
}
 

   

 


# ****************************************************
#            ---- CZESC DYNAMICZNA MAPY     ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
dynamicMapPart = function(chosendate){
    # UWAGA FUNKCJA AKTUALNIE NIE JEST U¯YWANA!!
    
    dynamicElements = dynamicMapElements(chosendate)
    
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
    map = leafletProxy(mapId = "covMap", data = data2map_norm_radius[,coordCols]) %>%
        clearMarkers() %>%
        addCircleMarkers(
            radius = data2map_norm_radius$confirmedRadius, 
            weight = mapDesignOpts$weight, 
            color = mapDesignOpts$color, 
            label = HTML_labels2map, 
            labelOptions = labelOptionsList
        )

    return(map)    
}





# LITERATURA / TUTORIALE
# https://rstudio.github.io/leaflet/shiny.html








if (!require("shiny")) {
    install.packages("shiny")
    # library("shiny")
}

if (!require("shinydashboard")) {
    install.packages("shinydashboard")
    # library("shiny")
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           ---- WCZYTANIE PAKEITÓW  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Pakiety dane covid-19
# Poniewa¿ pakietu covdata nie ma w repozytorium cran trzeba dodaæ repozytorium
# twórcy prof Kieran’a Healy

if (!require("drat")) {
    install.packages("drat")
    library("drat")
}

# Po dodaniu repozytorium kjhealy z gitHuba mo¿na instalowac covdata jak zwyk³y
# pakiet repozytorium Cran
drat::addRepo("kjhealy")

if (!require("covdata")) {
    install.packages("covdata")
    library("covdata")
}


# ~~~~~~~~~~~~~~~~~~~~~~~~
# Pakiety dotyczace map
# ........................

if (!require('leaflet')) {
    install.packages('leaflet')
    library("leaflet")
}

# magrittr --> dla funkcji %>%
# if (!require('magrittr')) {
#     install.packages('magrittr')
#     library("magrittr")
# }


##

if (!require('rworldmap')) {
    install.packages('rworldmap')
    library(rworldmap)
}

if (!require('rgeos')) {
    install.packages('rgeos')
    library(rgeos)
}


renderCovidMap <- function() {
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #       ----   EKSTRAKCJA DANYCH covid-19  ----
    # 
    # z pakietu covdata
    # wersja dla skumulowanej liczby przypadkóW
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    # 
    #  TODO 
    #  zautoamtyzowaæ otrzymywanie bie¿acej daty
    currentdate = '2020-05-09'
    covnat.actual = covnat[covnat$date == currentdate, ]
    
    covnat.actual = covnat.actual[order(covnat.actual$iso3), ]
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ----  ZMAPOWANIE DANYCH DO KOORDYNAT KRAJOW     ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # WCZYTANIE KOORDYNAT KRAJOW I WYZANCZENIE CENTROIDOW - w przyszlosci mozna
    # zastapic to wczytaniem pliku konfiguracyjnego z koordynatami pañstw
    worldmap <- getMap(resolution="high")
    countriesCentroids <- gCentroid(worldmap, byid=TRUE)
    
    
    cISO3.rwmap = (worldmap@data[,c('ADMIN','ISO3')])
    cISO3.rwmap = cISO3.rwmap[order(cISO3.rwmap$ISO3),]
    
    # INFO ma jedynie znaczenie informacyjne ostateczneio mo¿na bedzie to usunac
    INFO = list()
    if (length(cISO3.rwmap$ISO3) == length(unique(cISO3.rwmap$ISO3))){
        INFO$iso3.unique = 'Wartoœci kodóW ISO3 dla  tabeli cISO3.rwmap s¹  unikalne'
    }
    #INFO
    
    
    # DOPASOWANIE POMIÊDZY WYBRANYMI DANYMI A KOORDYNATAMI CENTROW PANSTW
    #  dopasowanie gdzie kraje zawarte w cISO3.rwmap znajduj¹ siê w macierzy koordynat krajów countriesCentroids@coords
    matching1 = match(rownames(countriesCentroids@coords), cISO3.rwmap$ADMIN)
    
    #  dodanie do ramki danych z centroidami krajów kolumny z kodem ISO3
    countryCentr_DF = as.data.frame(countriesCentroids@coords)
    countryCentr_DF = cbind(countryCentr_DF, as.character(cISO3.rwmap$ISO3[matching1]))
    colnames(countryCentr_DF)[3] = 'ISO3'
    
    
    #  Wyci¹gniêcie tylko krajów które s¹ obecne w ramce danych koronawirusa covnat.actual
    matching = match(covnat.actual$iso3, countryCentr_DF$ISO3)
    countryCentr_DF.req = countryCentr_DF[matching, ]
    
    
    #  Ile jest wartoœci NA
    INFO$not.matched.countries = sum(as.numeric(is.na(matching)))
    
    
    # Sprawdzenie poprawnoœci mapowania (kolejnoœci):
    bool1 = countryCentr_DF.req$ISO3 == covnat.actual$iso3
    
    INFO$perfect.match = paste('liczba poprawnie dopasowanych pañstw: ',
                               sum(as.numeric(bool1[!is.na(bool1)])))
    INFO$covid.current.data.size = dim(covnat.actual)
    
    
    # STWORZENIE MAPY PRZYPADKOW
    
    colnames(countryCentr_DF.req)[1:2] = c('Long', 'Lat')
    
    maxRadius = 0.5*10^2
    maxValue = 10^7
    
    
    data2Map = sqrt(covnat.actual$cu_deaths)
    # data2Map[data2Map > maxValue]  = maxValue
    
    # Liczba przypadków proporcjonalna do pola powierzchii
    data2Map = sqrt(data2Map / maxValue)
    data2Map = data2Map*maxRadius
    # data2Map[data2Map > (maxValue/100)] = (maxValue*100)
    
    
    map <- leaflet(data = countryCentr_DF.req[1:2], options = leafletOptions(minZoom = 2, maxZoom = 7))
    map <- setView(map, lng = 15, lat = 45, zoom = 4)
    # map %>% addTiles()
    map <- addProviderTiles(map, provider = providers$CartoDB.Voyager)
    map %>% addCircleMarkers(radius = data2Map*maxRadius, weight = 1, color = '#00b3b3')
    
}

# 
# map = renderCovidMap()
# renderCovidMap()
# renderLeaflet(map)



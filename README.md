# myrepo
## Gałąź: covid19-maps

### Zawiera pliki:
+ serwer.R, ui.R - demonstracja działania mapy
+ mapsFiles/covid-maps.R - plik głównie zawierający funkcje
	+ addActiveCases() - *podfunkcja  funkcji dynamicMapElements()* <br>
	oblicza aktywne przypadki na podstawie kolumn confirmed, recovered i deaths
	
	+ computeRadius() -  *podfunkcja  funkcji dynamicMapElements()* <br> przelicza dane na promień okęgu (liczba przypadków proporcjonalna do powierzchnii)
	
	+ initializeWorldMap() - inicjalizuje warstwę mapy
	
	+ dynamicMapElements() - wyciaga dane z pomocą pakietu COVID19 i tworzy potrzebne dane do mapy z 
+ mapsFiles/countryiesPl.rds - plik ze spolszczeniem nazw państw
+ www/mapStyles.css - plik css ze stylami elementow mapy


#### 31.08.2020  Co zmieniono/ dodano:
+ Dodano dane sumaryczne przypadków Covid-19, aktualne przypadki i śmierci 
+ Dodano spolszczenie nazw państw
+ Zmieniono normalizacje na 1mln polpulacji danego kraju
+ Poprawiono lokalizację niektórych markerów kołowych
+ Zmieniono położenie przycisków sterujących mapą


#### 08.09.2020  Co zmieniono/ dodano:
Dodano mape Polski z danymi przypadków i zgonów w województwach i potrzebne do niej dane i funckje
+ Pobieranie

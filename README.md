# myrepo
## Gałąź: covid19-maps

Zawiera pliki:
+ covid-maps.R - plik głównie zawierający funkcje
	+ addActiveCases() - *podfunkcja  funkcji dynamicMapElements()* <br>
	oblicza aktywne przypadki na podstawie kolumn confirmed, recovered i deaths
	
	+ computeRadius() -  *podfunkcja  funkcji dynamicMapElements()* <br> przelicza dane na promień okęgu (liczba przypadków proporcjonalna do powierzchnii)
	
	+ initializeWorldMap() - inicjalizuje warstwę mapy
	
	+ dynamicMapElements() - wyciaga dane z pomocą pakietu COVID19 i tworzy potrzebne dane do mapy z 
	
+ app - demonstracja działania mapy
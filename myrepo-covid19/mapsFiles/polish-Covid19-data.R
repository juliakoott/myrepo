# ! encoding: utf-8


# TODO: sprawdzic poprawnosc wartosci skumulowanych! Sprawdzic czy dalej dane sa zgodne!
# TODO: DODAC DLUGOSC I SZEROWKOSC GEOGRAFICZNA DO TABELI Z DANYMI PL!
# 3. Dodac ogranicznie wczytanych tabel do zakresu data poczatkowa mininalna data koncowa
# 
# 
# DONE:
# TODO: porownanie czy maksymalnma data jest wyzsza od ostatnio zapisanej w danych


# https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r#:~:text=Answer%20%3A%20Use%20%5B%5E%5B%3Aalnum,in%20regex%20or%20regexpr%20functions.
removeBetweenBrackets <- function(strX) {
	# Usuniecie tekstu pomiedzy nawiasami [] 
	# strX - wektor typu character
	strX = gsub(x = strX, pattern = " ", replacement = "")
	strX = gsub(x = strX, pattern = "#", replacement = "&%&")
	
	# https://stackoverflow.com/questions/44563835/replacing-square-brackets-with-curly-brackets-in-r
	strX = gsub(x = strX, pattern = "\\[", replacement = "#")
	strX = gsub(x = strX, pattern = "\\]", replacement = "#")
	
	strX = gsub(x = strX, pattern = "#.*#", replacement = "")
	strX = gsub(x = strX, pattern = "&%&", replacement = "#")
	
	return(strX)
}


updateCovid19PL <- function() {## POCZATEK FUNKCJI -----
   
   #  ----- Plan: -----
   #' 1. Pobrac stronę i przekonwertować tabele na ramki danych
   #' 
   #' 2. Przekonwertowac daty na typ Date i sprawdzic czy sa nowe dane
   #' 
   #' 3. Jesli sa nowe dane to kontynuowac jesli nie to przerwac sprawdzic za godzine
   #' tutaj bedzie potrzebny zmienna konfiguracyjna do zapisywania kiedy ostatnio dany zostaly pobrane!
   #' ta informacja powinna byc tez zapisywana w pliku rds z danymi 
   #' 
   #' 4. Wyczyscic bledy:
   #' 			 + usunac tekst pomiedzy kwadratowymi nawiasami 
   #' 			 + przekonwertowac stringi na liczby
   #' 			 +
   #' 5. Polaczyc obie tabelki tak aby były zgodne z tabelka z pakietu COVID19
   
   # covid.data.pl covid.data.pl
   
   ## Wymagane pakiety: reshape2, rvest
   library(rvest)
   library(reshape2)
   
   
   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ## 	-----  1. Pobrac stronę i przekonwertować tabele na ramki danych  ----- 
   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   ## https://stackoverflow.com/questions/31176709/load-a-table-from-wikipedia-into-r
   
   ## Odczytanie tabel z URL
   dataURL <- "https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/Poland_medical_cases_by_voivodeship"
   disposable = read_html(x = dataURL)
   disposable =  rvest::html_nodes(x = disposable, css = "table") # temp --> disposable
   
   
   ## Konwersja tabel html do ramki danych
   covid.data.pl = list() # covid.data.pl --> covid.data.pl
   
   covid.data.pl$confirmed = as.data.frame(html_table(disposable[1]))
   covid.data.pl$deaths = as.data.frame(html_table(disposable[2]))
   # all(covid.data.pl$confirmed==covid.data.pl$confirmed)
   
   last_date = as.Date(c(NA,NA))
   for (index in 1:2) {
	   	
	   	## Wstępne posprzatanie ramki danych 
	   	colnames(covid.data.pl[[index]]) = covid.data.pl[[index]][1, ]
	   	covid.data.pl[[index]] = covid.data.pl[[index]][-1, ]
	   	
	   	df_colnames = colnames(covid.data.pl[[index]])
	   	colnames(covid.data.pl[[index]]) = c('date', df_colnames[-1])
	   	
	   	ncols = ncol(covid.data.pl[[index]])
	   	which.col = which("Sources" == colnames(covid.data.pl[[index]]))
	   	covid.data.pl[[index]] = covid.data.pl[[index]][ ,-which.col]
	   	
	   	
	   	## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	   	## 	-----  2. Przekonwertowac daty na typ Date i sprawdzic czy sa nowe dane  ----- 
	   	## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      	
    	# Ostatnia kolumna to suma brzegowa!
    	## Zalozenia: daty sa ulozone rosnaco data jest w 1-szej kolumnie
    	date_col = 1
    	Sys.setlocale("LC_TIME", "English") # zmiana formatu czasu na angielski inaczej as.Date() sobie nie poradzi z angielska nazwa miesiaca
    	covid.data.pl[[index]][,date_col] = as.Date(covid.data.pl[[index]][,date_col], format = "%d %B %y")
    	
    	last_date_idx = which.max(covid.data.pl[[index]][,date_col])
    	last_date[index] = covid.data.pl[[index]][last_date_idx,date_col]
    	
    	covid.data.pl[[index]] = covid.data.pl[[index]][1:last_date_idx,]
   }
   
   last_date = min(last_date)
      
   ## 	-----  3. Jesli sa nowe dane to kontynuowac jesli nie to przerwac sprawdzic za godzine  -----   
   ## Zamiast poniższego kodu moglaby byc funkcja  loadCovid19PL()
   if(!exists('.Covid19PL_DataObj', envir = .GlobalEnv)){
      
      # SPRAWDZENEI CZY  WYMAGANY PLIK Z DANYMI ISTNIEJE W KATALOGU mapsFiles/
      if (length(Sys.glob('mapsFiles/covid19_DataPL.rds')) == 0) {
         #  Jesli nie ma stworzenie go:
         # https://stackoverflow.com/questions/41553480/declare-variable-with-a-dot-at-the-begining-in-r
         .Covid19PL_DataObj = list()
         .Covid19PL_DataObj$info = list(date = as.Date('1800-01-01')) # ustawienie jakiejs dawnej daty
         .Covid19PL_DataObj$data = data.frame()
         
      } else {
         print("Wczytuje dane")
         .Covid19PL_DataObj = readRDS('mapsFiles/covid19_DataPL.rds')
         properData = all(names(.Covid19PL_DataObj) %in% c('info', 'data'))
         
         if (!properData) {
            print("Niewlasciwe dane")
            .Covid19PL_DataObj = list()
            
            .Covid19PL_DataObj$info = list(
               date = as.Date('1800-01-01'), # ustawienie jakiejs dawnej daty
               lastUpdateAttempt = Sys.time()
            ) 
            .Covid19PL_DataObj$data = data.frame()
         }
      }
   } else {
     get('.Covid19PL_DataObj', envir = .GlobalEnv)
     time_diff = as.numeric(Sys.time()) - as.numeric(.Covid19PL_DataObj$info$lastUpdateAttempt)
      if (time_diff < 60) {
        print("Pomijam ten update z poniewaz ostatni byl mniej niz 60sek temu")
      }
   }
   
   # zapisanie ostatniej proby aktualizacji
   .Covid19PL_DataObj$info$lastUpdateAttempt = Sys.time()
   print(".Covid19PL_DataObj$info$lastUpdateAttempt")
   print(.Covid19PL_DataObj$info$lastUpdateAttempt)
   
   print(paste("last_date:", as.character(last_date)))
   cat(".Covid19PL_DataObj$info")
   print(.Covid19PL_DataObj$info)
   
   updateData = F
   if ( last_date > .Covid19PL_DataObj$info['date']) {
      # aktualizacja tylko jesli w obu tabelkach sa nowe dane 
      updateData = T
      library(reshape2)
   }
   
   
   
   if(!updateData) {
     assign('.Covid19PL_DataObj', .Covid19PL_DataObj, envir = .GlobalEnv)
     return(updateData)
     
   }else { # if(updateData) 
      # covid19_pl_file$data
      
      
      
    covid.data.pl_copy = as.list(rep(NA,2))
    names(covid.data.pl_copy) = names(covid.data.pl) 
    for (index in 1:2) {
      
      # ---- POCZATEK PETLI ----
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## 			-----  4. Poprawienie tabeli i Wyczyszczenie bledow  ----- 
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # # Zmiana nazw wojewodztw na polskie
      colnames.pl = data.frame(
      	ang = c(
      		"Lower Silesia (DS)", "Kuyavia-Pomerania (KP)", "Lubusz (LB)", "Łódź (LD)",             
      		"Lublin (LU)", "Lesser Poland (MA)", "Masovia (MZ; Warsaw)", "Opole (OP)", "Podlaskie (PD)",
      		"Subcarpathian (PK)", "Pomerania (PM)", "Holy Cross (SK)", "Silesia (SL)", "Warmia–Masuria (WN)",
      		"Greater Poland (WP)", "West Pomerania (ZP)", "Poland daily", "Poland total"
      	),
      	pol = c(
      		"Dolny Śląsk", "Kujawsko-Pomorskie", "Lubuskie", "Łódzkie",             
      		"Lubelskie", "Małopolska", "Mazowieckie", "Opolskie", "Podlaskie",
      		"Podkarpackie", "Pomorskie", "Świętokrzyskie", "Śląskie", "Warmińsko-Mazurskie",
      		"Wielkopolska", "Zachodnio-Pomorskie", "dzienne", "cumulatywnie"
      	),
      	code = c(
      	  "DS","KP","LB","LD","LU","MA","MZ","OP","PD","PK",
      	  "PM","SK","SL","WN","WP","ZP","PolDaily","PolCumul"
        ),
      	stringsAsFactors = F
      )
      
      
      names.match = match(colnames.pl$ang, colnames(covid.data.pl[[index]]))
      colnames(covid.data.pl[[index]])[names.match[!is.na(names.match)]] = colnames.pl$pol[!is.na(names.match)]
      
      # Usuniecie tekstu pomiedzy kwadratowymi nawiasami 
      
      # covid.data.pl.copy = list()
      # covid.data.pl.copy$confirmed = covid.data.pl[[index]]
      
      # Usuniecie spacji
      # https://stackoverflow.com/questions/26803323/how-to-convert-a-string-into-numeric-values-in-r
      covid.data.pl[[index]][,-1] = apply(covid.data.pl[[index]][,-1], 2, removeBetweenBrackets)
      removeBetweenBrackets(covid.data.pl[[index]]$confirmed$date)
      
      where.space = covid.data.pl[[index]][,-1] == ""
      covid.data.pl[[index]][,-1][where.space] = "0"
      
      covid.data.pl_copy[[index]] = covid.data.pl[[index]]
      
      # Konwersja na typ danych numeric
      covid.data.pl[[index]][,-date_col] = apply(covid.data.pl[[index]][,-date_col], 2, as.numeric)
      
      ## TODO:  ZROBIĆ PRZETWARZANIE TYCH STRINGOW "1-1" DO LICZBY!
      # where.na = is.na(covid.data.pl[[index]])
      # text_2_eval = as.character(covid.data.pl_copy[[index]][where.na])
      # 
      # text_2_eval = c("1-1","1-1")
      # 
      # values = numeric(length(text_2_eval))
      # for (ind1 in 1:length(text_2_eval)) {
      #   strX = text_2_eval[ind1]
      #   print(strX)
      #   values[ind1] = eval(parse(text=as.character(strX)))
      #   # parse(text="")
      # }
      # 
      # covid.data.pl[[index]][where.na] = values
      
      
      # POKI CO TAK JAK PONIZEJ
      where.na = is.na(covid.data.pl[[index]])
      covid.data.pl[[index]][where.na] = 0
      # print(covid.data.pl_copy[[index]][where.na])
       
      ### sprawdziłem poprawnosc sumy brzegowej dla Kujawsko-Pomorskie do daty  19 June 2020 !
      ### wszystko było zgodne!
      
      # ---- KONIEC PETLI ----
    }
    
    #  ---- dotad ----
    
    ## 5.1 Dodanie do tabeli o wyzszej minimalnej dacie brakujacych wierszy
    ## Jest to potrzebne poniewaz zgony zaczeto wprowadzac pozniej do tabelki
    
    # min_date = sapply(covid.data.pl, function(X) { min(X$date) })
    # as.Date(min_date, origin = "1970-01-01")
    
    ncols = ncol(covid.data.pl[[1]])
    nlacks = 8 # brakujace wiersze
    
    bind_matrix = as.data.frame(matrix(numeric(nlacks*ncols), ncol = ncols))
    bind_matrix[, date_col] = covid.data.pl[[1]]$date[1:nlacks]
    colnames(bind_matrix) = colnames(covid.data.pl[[1]])
    
    extended.covid.data.pl = rbind(bind_matrix, covid.data.pl[[2]])
    # head(extended.covid.data.pl, n = 30)
    covid.data.pl[[2]] = extended.covid.data.pl
    
    date_range = c(min(covid.data.pl[[2]]$date), last_date)
    print('date_range');  print(date_range) # TODEL
    
    # przed obliczneiem wartosci kumulatywnych sprawdzic czy kolumny sa zgodne i czy data jest zgodna !
    sortedDate = !is.unsorted(covid.data.pl[[1]]$date)
    consistentDate = all(covid.data.pl[[1]]$date == covid.data.pl[[2]]$date)
    consistentCols = all(colnames(covid.data.pl[[1]]) == colnames(covid.data.pl[[2]]))
    
    # TODO: Dodac uzgodnianie kolejnosci kolumn i wierszy jesli okaze sie ze nie
    # sa poukladane zgodnie. N razie tylko ostrzezenie i przerwanie funkcji
    if ( !(consistentDate & sortedDate & consistentCols) ) {
       warning('Niezgodne ulozenie daty pomiedzy tabelami w trakcie roby polaczenia tabeli przypadkow i smierci!')
       return()
    }
    
    # W tym miejscu ulozenie wierszy pomiedzy 
       
    ## 5. Oblczenie wartości kumulatywnych dla danych ----
    covid.data.pl_old = covid.data.pl
    covid.data.pl = list()
    
    # covid_cumul.data.pl = list()
    k=1
    for (index in 1:4) {
      print(index)
      if (index %% 2 == 1) {
        # nowe przypadki
        covid.data.pl[[index]] = covid.data.pl_old[[k]]
        names(covid.data.pl)[index] = paste0(names(covid.data.pl_old)[k], "New")
        
      }else {
        # kumulatytwne przypadki
        print(names(covid.data.pl)[index-1])
        covid.data.pl[[index]] = cbind.data.frame(
          date = covid.data.pl[[index-1]][, date_col],
          cumsum(covid.data.pl[[index-1]][, -date_col])
        )
        names(covid.data.pl)[index] = names(covid.data.pl_old)[k]
        k = k+1
      }
    }
    
    # # Sprawdzenie czy jest w porzadku
    # all(cumsum(covid.data.pl$confirmedNew[,-1]) == covid.data.pl$confirmed[,-1])
    # all(cumsum(covid.data.pl$deathsNew[,-1]) == covid.data.pl$deaths[,-1])
    
    # TODO: sprawdzic poprawnosc wartosci skumulowanych! Sprawdzic czy dalej dane sa zgodne!
    # TODO: DODAC DLUGOSC I SZEROWKOSC GEOGRAFICZNA DO TABELI Z DANYMI PL!
    
    # MOZNA to zrobic tworzac osobna tabele z dlugoscia i osobna tabele z
    # szerokoscia geograficzna a nastepnie przekonwertowac te tabele funkcja
    # reshape2::melt() i polaczyc tak jak reszte tabel/ ramek danych
    
    ## 6.0 Dodać Tabelki z długością, szerokością geograficzną, populacja wojewodztw  ----
    
    addit_info_Pl = readRDS(file = 'mapsFiles/additInfoDataPl.rds')
    covid.data.pl_copy2 = covid.data.pl
    covid.data.pl_dispos = covid.data.pl
    # append(x=covid.data.pl_dispos, values=list(1:30))
    
    # covid.data.pl
    empty_df = covid.data.pl_dispos[[1]] # TODO:  #ZOPTYMALIZOWAC empty_df
    empty_df[,-1] = NA
    
    n_items = length(covid.data.pl_dispos)
    shift_addit = 2
    if (all(colnames.pl$code == addit_info_Pl$viovio_code)) {
      for (index in 1:(ncol(addit_info_Pl) - 2) ) {
        dispos_df = empty_df
        
        addit_index = shift_addit + index
        ind_vector = 2:ncol(empty_df)
        for (ind1 in ind_vector) {
          
          dispos_df[, ind1] = rep(addit_info_Pl[ind1-1,addit_index])
          
        }
        insert_name = colnames(addit_info_Pl)[addit_index]
        # apply(, 2, )
        
        covid.data.pl_dispos[[index + n_items]] = dispos_df
        names(covid.data.pl_dispos)[index + n_items] = insert_name
      }
      
    }# TODO ELSE  
    
    
    
    ## 6. Polaczyc obie tabelki tak aby byly zgodne z tabelka z pakietu COVID19  ----
    ##
    ## Polaczenie tabelek przypadkow i smierci w jedna tabele
    ## 
    ## kolumny: date, confirmed, confirmedNew, deathsNew, latitude, longitude,
    ## population (w planach)
    ## 
    ## co jest w pakiecie COVID19: date, tests, confirmed, recovered, deaths,
    ## hosp,	vent,	icu, population
    
    # Zmiana wymiarów ramki danych:
    # https://www.tutorialspoint.com/r/r_data_reshaping.htm
    
    #  W tym momencie dane powinny byc zgodne pod wzgledem liczby wierszy kolumn
    #  i ich ulozenia !!
    ## library(reshape2)
    
    
    tables_number = length(covid.data.pl_dispos) # 
    
    binded_list = as.list(rep(NA, tables_number))
    names(binded_list) = names(covid.data.pl_dispos)
    # Liczba kolumn w przeksztalconej DF (dod.: data, nazwy wojewodztw):
    variable_count = tables_number + 2 
    # Liczba wierszy w nowej ramce danych:
    nrow_binded = (ncol(covid.data.pl[[1]])-1)*nrow(covid.data.pl[[1]]) # jedna kolumna to id (klucz glowny)

    # # binded_df --> macierz w ktorej beda 8 tabele/ramki danych (confirmed, confirmedNew, deathsNew)
    # # ponizej: Jak wydajnie stworzyc macierz:
    # # https://stackoverflow.com/questions/1745622/best-way-to-allocate-matrix-in-r-null-vs-na
    binded_df = matrix()
    length(binded_df) = nrow_binded*variable_count
    dim(binded_df) = c(nrow_binded, variable_count)
    
    
    {binded_df = as.data.frame(binded_df)
    cols_names = names(covid.data.pl_dispos)
    colnames(binded_df) = c('date', 'voivodeship', cols_names)
    cols_names = colnames(binded_df)}
    
    remove(dispos_df) # dla pewnosci
    
    for (index in 1:tables_number) {
      
      dispos_df = melt(data = covid.data.pl_dispos[[index]], id.vars = c('date'))
      colnames(dispos_df)[2:3] = c('voivodeship', names(covid.data.pl_dispos)[index])
      
      #REMOVE ::
      if (names(covid.data.pl_dispos)[index] == names(binded_list)[index]) { #REMOVE
        print(
          paste("zgodne nazwy dla",names(binded_list)[index])
        )
        binded_list[[index]] = dispos_df                  
      }
      #::REMOVE
      
      if ( index==1 ) {
        # binded_df[,'date'] = as.Date(binded_df[,'date'])
        binded_df[,'date'] = dispos_df$date
        binded_df[,'voivodeship'] = dispos_df$voivodeship
      }
      
      # ZPRAWDZENIE CZY DATY I WOJEWODZTWA SA ZGODNE
      if (index!=1 ) {
        consistentDate = all(binded_df[,'date'] == dispos_df$date)
        consistentVoivio = all(binded_df[,'voivodeship'] == dispos_df$voivodeship)
        if (!consistentVoivio | !consistentDate) {
           warning('niezgodne klucze podczas proby skladania danych')
           return()
          
        } else{
           print("wszystko zgodne")
        }
      }
      
      col_name = names(covid.data.pl_dispos)[index] 
      binded_df[, col_name] = dispos_df[,col_name]
      remove(dispos_df) # na czas rozwijania funkcji potem mozna uwsunac
    }
    
    
    # saveRDS('mapsFiles/covid19_DataPL.rds', object = covid19_pl_file)
    # remove(covid19_pl_file)
    
    # AKTUALIZACJA:: ZAPISANIE DANYCH DO PLIKU
    .Covid19PL_DataObj$info$date = as.Date(last_date) #
    .Covid19PL_DataObj$info$lastUpdateAttempt = Sys.time()
    
    .Covid19PL_DataObj$data = binded_df
    .Covid19PL_DataObj$meta$uniqe_date = unique(.Covid19PL_DataObj$data$date)
    
    
    if (is.unsorted(.Covid19PL_DataObj$meta$uniqe_date)){
      .Covid19PL_DataObj$meta$uniqe_date = sort(.Covid19PL_DataObj$meta$uniqe_date)
      
    }
    
    # ZAPIS DO PLIKU
    saveRDS('mapsFiles/covid19_DataPL.rds', object = .Covid19PL_DataObj)
    
    assign('.Covid19PL_DataObj', .Covid19PL_DataObj, envir = .GlobalEnv)
    return(updateData)
  }
} ## KONIEC FUNKCJI -----

covid19PL <- function(chosen_date = Sys.Date()-1) {
  get(".Covid19PL_DataObj", envir = .GlobalEnv)
  indexes = which(.Covid19PL_DataObj$data$date == chosen_date)
  if (length(indexes) == 0) {
    dates_N = length(.Covid19PL_DataObj$meta$uniqe_date)
    chosen_date = .Covid19PL_DataObj$meta$uniqe_date[dates_N]
    indexes = which(.Covid19PL_DataObj$data$date == chosen_date)
    
    warning_info = paste("wybrana data jest spoza zakresu wybrano maksymalna dostpna date date", as.character(chosen_date))
    warning(warning_info)
  }
  # TODO: dodac instrukcje warunkowe dla przypadku kiedy nie ma daty w danych
  chosen_Data = .Covid19PL_DataObj$data[indexes, ]
  return(chosen_Data)
}

# Wlasciewie ta funkcja jest niepotrzebna bo o samo wykonuje funkcja
# updateCovid19PL()
loadCovid19PL <- function() {
   if(!exists('.Covid19PL_DataObj', envir = .GlobalEnv)){
      # SPRAWDZENEI CZY  WYMAGANY PLIK Z DANYMI ISTNIEJE W KATALOGU mapsFiles/
      if (length(Sys.glob('mapsFiles/covid19_DataPL.rds')) == 1) {
         #  Wczytanie danych z pliku
         Covid19PL_DataObj = readRDS('mapsFiles/covid19_DataPL.rds')
         assign(x = '.Covid19PL_DataObj', value=Covid19PL_DataObj, envir=.GlobalEnv)
         
         return(TRUE)

      }else if (length(Sys.glob('mapsFiles/covid19_DataPL.rds')) == 0) {
         warning("! Najpierw uruchom funkcje updateCovid19PL() !")
         
      }else {
         warning("nieznany blad")
      }
   }
}




# ==========================  NIEPOTRZEBNE FUNKCJE  =====================================

## ! Funkcja niepotrzebana dzieki funkcji shiny reactiveTimer() 
checkUpdateCovid19PL <- function() {
	# Proces ktory powinien być w aplikacji Shiny
	if (exists(".Covid19PL_DataObj", envir=.GlobalEnv)) {
		time_difference = as.numeric(Sys.time()) - as.numeric(.Covid19PL_DataObj$info$lastUpdateAttempt) # szybsza wersja
		if (as.numeric(time_difference) > 3600) { # sprawdzanie co godzine czy jest aktualizacja
			print("aktualizuje dane")
			updateCovid19PL()
			# loadCovid19PL()   
		}
	}else {
		print("aktualizuje dane")
		updateCovid19PL()
		# loadCovid19PL()
	}
}



# 
# .Covid19PL_DataObj$info$lastUpdateAttempt = Sys.time()
# 
# updateCovid19PL()

# # tescik
# datasy = readRDS('mapsFiles/covid19_DataPL.rds')
# datasy$info = as.Date("2020-08-12")
# 
# saveRDS('mapsFiles/covid19_DataPL.rds',object = datasy)
# remove(.Covid19PL_DataObj)
# updateCovid19PL()







# # Po wykonaniu kodu czas (godzina) jego wykonania powinien byc gdzies zapisany.
# # Jesli dane nie zostaly zaktualizowane nastepna proba aktualizacji powinna
# # byc np za godzine



# 
# 
# html_table(disposable[1]) 
# html_table(disposable[2]) 
# 
# 
# 
# temp <- dataURL %>% 
# 	read_html %>%
# 	html_nodes("table")
# 
# # html_table(temp) ## Just the "legend" table
# html_table(temp[1]) 
# html_table(temp[2]) 

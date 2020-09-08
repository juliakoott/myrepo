t=unique(covid19()[1]) 
library(COVID19)
library(plotly)
#wczesniej prosze daj, ?e kraj to zmienna wybrana z comboxa :) Besos!
d<-covid19()
#kraj=input$kraj
  #as.character(kraj)
#m=d[which(d$id==kraj),]
kraj="GBR"
figzs <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#008000'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = m$deaths)
figzs <- figzs %>% layout(
  title = " Ogólna liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba zgonów'))

#dzienne dane
ys=m$deaths
ns=c()
for( i in 1:length(ys)){
  if(i==1){
    ns[i]=ys[1]
  }
  else if(i==length(ys)){
    ns[i]=ys[i-1]-ys[i-2]
  }
  else {
    ns[i]=ys[i]-ys[i-1]
  }
}
figz1s <- plot_ly(
  type = "bar",
  marker= list(
    color = '#008000'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = ns)
figz1s <- figz1s %>% layout(
  title = "Liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba zgonów'))

##liczba test?w 
d<-covid19()
kraj=as.character(kraj)
m=d[which(d$id==kraj),]
figts <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#800080'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = m$tests)
figts <- figts %>% layout(
  title = " Og?lna Liczba test?w przeprowadzonych w celu wykrycia choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba test?w'))

#dzienne dane
y1s=m$tests
n1s=c()
for( i in 1:length(y1s)){
  if(i==1){
    n1s[i]=y1s[1]
  }
  else if(i==length(y1s)){
    n1s[i]=abs(y1s[i-1]-y1s[i-2])
  }
  else  {
    n1s[i]=abs(y1s[i]-y1s[i-1])
  }
}

for( i in 2:length(n1s)){
  if(n1s[i]==0){
    n1s[i]=n1s[i-1]
  }
}

figt1s <- plot_ly(
  type = "bar",
  marker= list(
    color = '	#800080'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = n1s)
figt1s <- figt1s %>% layout(
  title = "Liczba test?w przeprowadzonych w celu wykrycia choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba test?w'))
## chorzy
d<-covid19()
kraj=as.character(kraj)
m=d[which(d$id==kraj),]
figcs <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#dc143c'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = m$confirmed)
figcs <- figcs %>% layout(
  title = " Og?lna liczba zdiagnozowanych przypadk?w choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba chorych'))

#dzienne dane
y2s=m$confirmed
n2s=c()
for( i in 1:length(y2s)){
  if(i==1){
    n2s[i]=y2s[1]
  }
  else if(i==length(y2s)){
    n2s[i]=abs(y2s[i-1]-y2s[i-2])
  }
  else {
    n2s[i]=abs(y2s[i]-y2s[i-1])
  }
}
figc1s <- plot_ly(
  type = "bar",
  marker= list(
    color = '#dc143c'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = n2s)
figc1s <- figc1s %>% layout(
  title = "Liczba wykrytych przypad?w choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba chorych'))

##wyzdrowiaych
d<-covid19()
kraj=as.character(kraj)
m=d[which(d$id==kraj),]
figos <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#800000'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = m$recovered)
figos <- figos %>% layout(
  title = " Og?lna liczba os?b wyzdrowiaych z  choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba ozdrowie?c?w'))

#dzienne dane
y3s=m$recovered
n3s=c()
for( i in 1:length(y3s)){
  if(i==1){
    n3s[i]=y3s[1]
  }
  else if(i==length(y3s)){
    n3s[i]=abs(y3s[i-1]-y3s[i-2])
  }
  else {
    n3s[i]=y3s[i]-y3s[i-1]
    n3s[i]=abs(n3s[i])
  }
}
figo1s <- plot_ly(
  type = "bar",
  marker= list(
    color = '#800000'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = n3s)
figo1s <- figo1s %>% layout(
  title = "Liczba wyzdowiaych os?b z choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba ozdrowie?c?W'))
## procent pozytywnych wynik?w test?w
y1s=m$tests
n1s=c()
for( i in 1:length(y1s)){
  if(i==1){
    n1s[i]=y1s[1]
  }
  else if(i==length(y1s)){
    n1s[i]=abs(y1s[i-1]-y1s[i-2])
  }
  else  {
    n1s[i]=abs(y1s[i]-y1s[i-1])
  }
}

for( i in 2:length(n1s)){
  if(n1s[i]==0){
    n1s[i]=n1s[i-1]
  }
}
y2s=m$confirmed
n2s=c()
for( i in 1:length(y2s)){
  if(i==1){
    n2s[i]=y2s[1]
  }
  else if(i==length(y2s)){
    n2s[i]=abs(y2s[i-1]-y2s[i-2])
  }
  else {
    n2s[i]=abs(y2s[i]-y2s[i-1])
  }
}
nps=c()
for (i in 1:length(n2s)){
  if (n2s[i]==0){
    nps[i]=0
  }
  else{
    nps[i]=(n2s[i]*100)/n1s[i]
  }
}

fig1s <- plot_ly(
  type = "bar",
  marker= list(
    color = '#8b008b'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = nps)
fig1s <- fig1s %>% layout(
  title = "Procent pozytwnych wynik?w na obecno?? wirusa SARS-COV-2 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Procent wynik?w [%]'))



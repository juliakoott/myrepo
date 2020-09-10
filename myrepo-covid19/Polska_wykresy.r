library(COVID19)
library(plotly)
d<-covid19()
m=d[which(d$id=="POL"),]
figz <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#008000'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = m$deaths)
figz <- figz %>% layout(
  title = " Ogólna liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
    xaxis = list(type = 'date',title = "Data"),
    yaxis = list(title = 'Liczba zgonów'))

y=m$deaths
n=c()
for( i in 1:length(y)){
  if(i==1){
    n[i]=y[1]
  }
  else if(i==length(y)){
    n[i]=y[i-1]-y[i-2]
  }
  else {
    n[i]=y[i]-y[i-1]
  }
}
figz1 <- plot_ly(
  type = "bar",
  marker= list(
    color = '#008000'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = n)
figz1 <- figz1 %>% layout(
  title = "Liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba zgonów'))


d<-covid19()
m=d[which(d$id=="POL"),]
figt <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#800080'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = m$tests)
figt <- figt %>% layout(
  title = " Ogólna Liczba testów przeprowadzonych w celu wykrycia choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba testów'))# testy sumaryczne



y1=m$tests
n1=c()
for( i in 1:length(y1)){
  if(i==1){
    n1[i]=y1[1]
  }
  else if(i==length(y1)){
    n1[i]=y1[i-1]-y1[i-2]
  }
  else  {
    n1[i]=y1[i]-y1[i-1]
  }
}

for( i in 2:length(n1)){
  if(n1[i]==0){
    n1[i]=n1[i-1]
  }
}

figt1 <- plot_ly(
  type = "bar",
  marker= list(
    color = '	#800080'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = n1)
figt1 <- figt1 %>% layout(
  title = "Liczba testów przeprowadzonych w celu wykrycia choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba testów'))# testy PL dzień



## chorzy
d<-covid19()
m=d[which(d$id=="POL"),]
figc <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#dc143c'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = m$confirmed)
figc <- figc %>% layout(
  title = " Ogólna liczba zdiagnozowanych przypadków choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba chorych'))


y2=m$confirmed
n2=c()
for( i in 1:length(y2)){
  if(i==1){
    n2[i]=y2[1]
  }
  else if(i==length(y2)){
    n2[i]=y2[i-1]-y2[i-2]
  }
  else {
    n2[i]=y2[i]-y2[i-1]
  }
}
figc1 <- plot_ly(
  type = "bar",
  marker= list(
    color = '#dc143c'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = n2)
figc1 <- figc1 %>% layout(
  title = "Liczba wykrytych przypadków choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba chorych')) 

d<-covid19()
m=d[which(d$id=="POL"),]
figo <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#800000'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = m$recovered)
figo <- figo %>% layout(
  title = " Ogólna liczba osób wyzdrowiaych z  choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba ozdrowieńców')) 


y3=m$recovered
n3=c()
for( i in 1:length(y3)){
  if(i==1){
    n3[i]=y3[1]
  }
  else if(i==length(y3)){
    n3[i]=y3[i-1]-y3[i-2]
  }
  else {
    n3[i]=y3[i]-y3[i-1]
    n3[i]=abs(n3[i])
  }
}
figo1 <- plot_ly(
  type = "bar",
  marker= list(
    color = '#800000'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = n3)
figo1 <- figo1 %>% layout(
  title = "Liczba wyzdowiaych osób z choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba ozdrowieńcóW')) 



y1=m$tests
n1=c()
for( i in 1:length(y1)){
  if(i==1){
    n1[i]=y1[1]
  }
  else if(i==length(y1)){
    n1[i]=y1[i-1]-y1[i-2]
  }
  else  {
    n1[i]=y1[i]-y1[i-1]
  }
}

for( i in 2:length(n1)){
  if(n1[i]==0){
    n1[i]=n1[i-1]
  }
}
y2=m$confirmed
n2=c()
for( i in 1:length(y2)){
  if(i==1){
    n2[i]=y2[1]
  }
  else if(i==length(y2)){
    n2[i]=y2[i-1]-y2[i-2]
  }
  else {
    n2[i]=y2[i]-y2[i-1]
  }
}
np=c()
for (i in 1:length(n2)){
  if (n1[i]==0){
    np[i]=0
  }
  else{
    np[i]=(n2[i]*100)/n1[i]
  }
}

fig1 <- plot_ly(
  type = "bar",
  marker= list(
    color = '#8b008b'),
  x = as.Date(m$date, format= "%Y-%m-%d"), 
  y = np)
fig1 <- fig1 %>% layout(
  title = "Procent pozytwnych wyników na obecnosć wirusa SARS-COV-2 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Procent wyników [%]'))

#do swiata nazwij zmiena ze skrotem selected
mzp=max(y) # ilosc zgonow
mop=max(y3)# ilosc wyzdrowialych
mchp=max(y2) #ilosc chorych

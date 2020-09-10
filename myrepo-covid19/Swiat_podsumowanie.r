library(COVID19)
library(plotly)
d<-covid19()
data<-unique(d$date)

data<-unique(d$date)
z<-c(rep(0,length(data)))#zgony
ch<-c(rep(0,length(data))) #chorzy
o<-c(rep(0,length(data))) #ozdrowiency
#246x196
dd=matrix(nrow=length(data),ncol=196)
for(i in 1:length(data)){
  dd[i,]<-which(data[i]==d$date)
}


for (i in 1: length(data) ){
 z[i]<-sum(d$deaths[dd[i,]])
 ch[i]<-sum(d$confirmed[dd[i,]])
 o[i]<-sum(d$recovered[dd[i,]])
}


figzss <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#008000'),
  x = data, 
  y = z)
figzss <- figzss %>% layout(
  title = " Ogólna liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba zgonów'))

figcss <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#dc143c'),
  x = data, 
  y = ch)
figcss <- figcss %>% layout(
  title = " Ogólna liczba zdiagnozowanych przypadków choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba chorych'))

figoss <- plot_ly(
  type = "scatter",
  mode = "lines",
  line = list(
    color = '#800000'),
  x = data, 
  y = o)
figoss <- figoss %>% layout(
  title = " Ogólna liczba osób wyzdrowiaylch z  choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba ozdrowienców'))

##dzienne
zs=c()
for( i in 1:length(z)){
  if(i==1){
    zs[i]=z[1]
  }
  else if(i==length(z)){
    zs[i]=z[i-1]-z[i-2]
  }
  else {
    zs[i]=z[i]-z[i-1]
  }
}
os=c()
for( i in 1:length(o)){
  if(i==1){
    os[i]=o[1]
  }
  else if(i==length(o)){
    os[i]=o[i-1]-o[i-2]
  }
  else {
    os[i]=o[i]-o[i-1]
  }
}
chs=c()
for( i in 1:length(ch)){
  if(i==1){
    chs[i]=ch[1]
  }
  else if(i==length(ch)){
    chs[i]=ch[i-1]-ch[i-2]
  }
  else {
    chs[i]=ch[i]-ch[i-1]
  }
}

figzss1 <- plot_ly(
  type = "bar",
  marker = list(
    color = '#008000'),
  x = data, 
  y = zs)
figzss1 <- figzss1 %>% layout(
  title = "Liczba zgonów spowodowanych chorob covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba zgonów'))

figcss1 <- plot_ly(
  type = "bar",
  marker = list(
    color = '#dc143c'),
  x = data, 
  y = chs)
figcss1 <- figcss1 %>% layout(
  title = " Liczba zdiagnozowanych przypadków choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba chorych'))
figoss1 <- plot_ly(
  type = "bar",
  marker = list(
    color = '#800000'),
  x = data, 
  y = os)
figoss1 <- figoss1 %>% layout(
  title = "Liczba osób wyzdrowialych z  choroby covid19 na przestrzeni dni",
  xaxis = list(type = 'date',title = "Data"),
  yaxis = list(title = 'Liczba ozdrowienców'))

mz=max(z) # ilosc zgonow
mo=max(o)# ilosc wyzdrowialych
mch=max(ch) #ilosc chorych

library(COVID19)
library(plotly)

 ## procent pozytywnych wynik?w test?w
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
  yaxis = list(title = 'Procent wynik?w [%]'))

#nowe procenciki
data<-unique(d$date)
z<-c(rep(0,length(data)))#zgony
ch<-c(rep(0,length(data))) #chorzy
o<-c(rep(0,length(data))) #ozdrowiency
#246x196
dd=matrix(nrow=length(data),ncol=196)
for(i in 1:length(data)){
  dd[i,]<-which(data[i]==d$date)
}
#rzedy czyli [i,] to dni

for (i in 1: length(data) ){
  z[i]<-sum(d$deaths[dd[i,]])
  ch[i]<-sum(d$confirmed[dd[i,]])
  o[i]<-sum(d$recovered[dd[i,]])
}
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
 pz<-c()
 po<-c()
 pch<-c()

 for (i in 1:length(n2)){
   if (n2[i]==0){
     pch[i]=0
    
   }
   else{
     pch[i]=((n2[i])/chs[i])*100
   }
 }
 for (i in 1:length(n3)){
   if (n3[i]==0){
     po[i]=0
     
   }
   else{
     po[i]=((n3[i])/os[i])*100
   }
 }
 
 for (i in 1:length(n)){
   if (n[i]==0){
     pz[i]=0
     
   }
   else{
     pz[i]=((n[i])/zs[i])*100
   }
 }
 fig2 <- plot_ly(
   type = "bar",
   marker= list(
     color = '#8b008b'),
   x = as.Date(m$date, format= "%Y-%m-%d"), 
   y = pz)
 fig2 <- fig2 %>% layout(
   title = "Procent zgonów wywolanych przez chorobe covid19 w Polsce w porówaniu ze zgonami na świecie na przestrzeni dni",
   xaxis = list(type = 'date',title = "Data"),
   yaxis = list(title = 'Procent wyników [%]'))
 fig3 <- plot_ly(
   type = "bar",
   marker= list(
     color = '#4b0082'),
   x = as.Date(m$date, format= "%Y-%m-%d"), 
   y = po)
 fig3 <- fig3 %>% layout(
   title = "Procent wyzdrowień z choroby covid19 w Polsce w porówaniu ze zgonami na świecie na przestrzeni dni",
   xaxis = list(type = 'date',title = "Data"),
   yaxis = list(title = 'Procent wyników [%]'))
 fig4 <- plot_ly(
   type = "bar",
   marker= list(
     color = '#ff00ff '),
   x = as.Date(m$date, format= "%Y-%m-%d"), 
   y = pch)
 fig4 <- fig4 %>% layout(
   title = "Procent  zdiagnozowanych przypadków choroby covid19 w Polsce w porówaniu ze zgonami na świecie na przestrzeni dni",
   xaxis = list(type = 'date',title = "Data"),
   yaxis = list(title = 'Procent wyników [%]'))
 
 
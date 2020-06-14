getwd()

#####################################################################
#Instalacja paczek

install.packages("ggplot2")
library(ggplot2)

install.packages("COVID19")
require("COVID19")

#####################################################################
#Wczytanie danych dla Polski

covid_PL = covid19("PL")
#covid_PL = cbind(covid_PL, covid_PL_dni)

covid_PL_dates = covid_PL[,2]
covid_PL_deaths = cbind(covid_PL_dates,covid_PL[,6])
covid_PL_infected = cbind(covid_PL_dates,covid_PL[,4])
covid_PL_recovered = cbind(covid_PL_dates,covid_PL[,5])


#####################################################################
#Total cases

y_deaths = data.matrix(covid_PL_deaths[,2])
x_deaths <- as.Date(covid_PL_deaths$date, "%Y/%m/%d")
y_infected = data.matrix(covid_PL_infected[,2])
x_infected <- as.Date(covid_PL_infected$date, "%Y/%m/%d")
y_recovered = data.matrix(covid_PL_recovered[,2])
x_recovered <- as.Date(covid_PL_recovered$date, "%Y/%m/%d")

plot(x_deaths,y_deaths,xlab="Czas", ylab="l. ofiar")
title("COVID19 | Polska | zgony")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

plot(x_infected,y_infected,xlab="Czas", ylab="l. zarażonych")
title("COVID19 | Polska | zarażeni")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

plot(x_recovered,y_recovered,xlab="Czas", ylab="l. wyleczonych")
title("COVID19 | Polska | wyleczeni")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

####################################################################
#Daily cases

covid_PL_infected_daily = c()
for(i in 2:(length(covid_PL_infected[,1]))) {
  covid_PL_infected_daily = c(covid_PL_infected_daily,(covid_PL_infected[i,2]-covid_PL_infected[i-1,2]) )
}
covid_PL_infected_daily = append(covid_PL_infected_daily,0,after = length(1))
covid_PL_infected_daily = cbind(covid_PL_dates,data.frame(covid_PL_infected_daily))


covid_PL_deaths_daily = c()
for(i in 2:(length(covid_PL_deaths[,1]))) {
  covid_PL_deaths_daily = c(covid_PL_deaths_daily,(covid_PL_deaths[i,2]-covid_PL_deaths[i-1,2]) )
}
covid_PL_deaths_daily = append(covid_PL_deaths_daily,0,after = length(1))
covid_PL_deaths_daily = cbind(covid_PL_dates,data.frame(covid_PL_deaths_daily))


covid_PL_recovered_daily = c()
for(i in 2:(length(covid_PL_recovered[,1]))) {
  covid_PL_recovered_daily = c(covid_PL_recovered_daily,(covid_PL_recovered[i,2]-covid_PL_recovered[i-1,2]) )
}
covid_PL_recovered_daily = append(covid_PL_recovered_daily,0,after = length(1))
covid_PL_recovered_daily = cbind(covid_PL_dates,data.frame(covid_PL_recovered_daily))

covid_PL_deaths = cbind(covid_PL_dates,covid_PL[,3])
covid_PL_infected = cbind(covid_PL_dates,covid_PL[,4])
covid_PL_recovered = cbind(covid_PL_dates,covid_PL[,6])


y_deaths_daily = data.matrix(covid_PL_deaths_daily[,2])
x_deaths_daily <- as.Date(covid_PL_deaths_daily$date, "%Y/%m/%d")
y_infected_daily = data.matrix(covid_PL_infected_daily[,2])
x_infected_daily <- as.Date(covid_PL_infected_daily$date, "%Y/%m/%d")
y_recovered_daily = data.matrix(covid_PL_recovered_daily[,2])
x_recovered_daily <- as.Date(covid_PL_recovered_daily$date, "%Y/%m/%d")

plot(x_deaths_daily,y_deaths_daily,xlab="Czas", ylab="l. ofiar na dzień")
title("COVID19 | Polska | zgony")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

plot(x_infected_daily,y_infected_daily,xlab="Czas", ylab="l. zarażonych na dzień")
title("COVID19 | Polska | zarażeni")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

plot(x_recovered_daily,y_recovered_daily,xlab="Czas", ylab="l. wyleczonych na dzień")
title("COVID19 | Polska | wyleczeni")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

####################################################################
#Tests performed (summary)

covid_PL_tests = cbind(covid_PL_dates,covid_PL[,3])

y_tests = data.matrix(covid_PL_tests[,2])
x_tests <- as.Date(covid_PL_tests$date, "%Y/%m/%d")
y_infected = data.matrix(covid_PL_infected[,2])
x_infected <- as.Date(covid_PL_infected$date, "%Y/%m/%d")

plot(x_tests,y_tests,xlab="Czas", ylab="l. przeprowadzonych testów")
title("COVID19 | Polska | testy")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Create data
data_tests <- data.frame(x_tests ,y_tests, y_infected)

# Barplot
blue <- rgb(0, 0, 1, alpha=0.5)
red <- rgb(1, 0, 0, alpha=0.5)

barplot(data_tests[,2], space=0, col=red)
par(new=TRUE)
barplot(data_tests[,3], space=0, col=blue)
title("COVID19 | Polska | Testy-Zarażeni (sumaryczny)")
legend("topleft",c("Testy","Zarażeni"), cex=0.6, fill=c(red,blue))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

####################################################################
#Tests performed (daily)
covid_PL_tests = cbind(covid_PL_dates,covid_PL[,3])

covid_PL_tests_daily = c()
for(i in 2:(length(covid_PL_infected[,1]))) {
  covid_PL_tests_daily = c(covid_PL_tests_daily,(covid_PL_tests[i,2]-covid_PL_tests[i-1,2]) )
}
covid_PL_tests_daily = append(covid_PL_tests_daily,0,after = length(1))
covid_PL_tests_daily = cbind(covid_PL_dates,data.frame(covid_PL_tests_daily))


y_tests_daily = data.matrix(covid_PL_tests_daily[,2])
x_tests_daily <- as.Date(covid_PL_tests_daily$date, "%Y/%m/%d")
y_infected_daily = data.matrix(covid_PL_infected_daily[,2])
x_infected_daily <- as.Date(covid_PL_infected_daily$date, "%Y/%m/%d")

plot(x_tests_daily,y_tests_daily,xlab="Czas", ylab="l. przeprowadzonych testów")
title("COVID19 | Polska | testy")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Create data
data_tests_daily <- data.frame(x_tests_daily ,y_tests_daily, y_infected_daily)

# Barplot
blue <- rgb(0, 0, 1, alpha=0.5)
red <- rgb(1, 0, 0, alpha=0.5)

barplot(data_tests_daily[,2], space=0, col=red)
par(new=TRUE)
barplot(data_tests_daily[,3], space=0, col=blue)
title("COVID19 | Polska | Testy-Zarażeni (dzienny)")
legend("topleft",c("Testy","Zarażeni"), cex=0.6, fill=c(red,blue))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

####################################################################
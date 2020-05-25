getwd()

#####################################################################
#Instalacja paczek

install.packages("data sets")
require("data sets")

install.packages("ggplot2")
require("ggplot2")

install.packages("GGally")
require("GGally")

install.packages("COVID19")
require("COVID19")
######################################################################


#ISO     |	vector of ISO codes to retrieve (alpha-2, alpha-3 or numeric). Each country is identified by one of its ISO codes
#level   |	integer. Granularity level. 1: country-level data. 2: state-level data. 3: city-level data.
#start   |	the start date of the period of interest.
#end     |	the end date of the period of interest.
#vintage |	logical. Retrieve the snapshot of the dataset at the end date instead of using the latest version? Default FALSE.
#raw     |	logical. Skip data cleaning? Default FALSE.
#cache   |	logical. Memory caching? Significantly improves performance on successive calls. Default TRUE.


# Worldwide data by country
covid19()
# Worldwide data by state
covid19(level = 2)
# US data by state
covid19("USA", level = 2)
# Swiss data by state (cantons)
covid19("CHE", level = 2)
# Italian data by state (regions)
covid19("ITA", level = 2)
# Italian and US data by city

#####################################################################
#Wczytanie danych dla Polski

covid_PL = covid19("PL")
#covid_PL = cbind(covid_PL, covid_PL_dni)

covid_PL_dates = covid_PL[,2]
covid_PL_deaths = cbind(covid_PL_dates,covid_PL[,3])
covid_PL_infected = cbind(covid_PL_dates,covid_PL[,4])
covid_PL_recovered = cbind(covid_PL_dates,covid_PL[,6])

y_deaths = data.matrix(covid_PL_deaths[,2])
x_deaths <- as.Date(covid_PL_deaths$date, "%Y/%m/%d")
y_infected = data.matrix(covid_PL_infected[,2])
x_infected <- as.Date(covid_PL_infected$date, "%Y/%m/%d")
y_recovered = data.matrix(covid_PL_recovered[,2])
x_recovered <- as.Date(covid_PL_recovered$date, "%Y/%m/%d")

#####################################################################
#Podstawowe ploty dla Polski

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
# Predykcja
#https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/


covid_PL = covid_PL[covid_PL[,4] > 0,]
covid_PL_dni = as.data.frame(c(1:nrow(covid_PL[,2])))
names(covid_PL_dni)[1] <- "dni"
covid_PL[,2] = covid_PL_dni

ggpairs(data=covid_PL, columns=c(4,2), title="COVID-19 PL")
model <- lm(confirmed ~  date, data = covid_PL)
summary(model)

ggplot(data=covid_PL, aes(model$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

ggplot(data = covid_PL, aes(x = date, y = confirmed)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

predykcja = data.frame(predict(model, covid_PL))

predykcja = predict(model,newdata = data.frame(date = c(64,65,66,67,68,69,70,71)))


#######
#Estimate = lm(date ~  confirmed, data = covid_PL)
#logEstimate = lm(date ~  log(confirmed), data = covid_PL)

#plot(covid_PL$confirmed,predict(Estimate),type='l',col='blue')
#lines(covid_PL$confirmed,predict(logEstimate),col='red')
#points(covid_PL$confirmed,covid_PL$date)

######
#http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/

# Build the model
model2 <- lm(confirmed ~ poly(date, 5, raw = TRUE), data = covid_PL)
# Make predictions
predykcja2 <- predict(model2)

plot(covid_PL$date,predict(model2),type='l',col='blue')
lines(covid_PL$date,predict(model),col='red')
points(covid_PL$date,covid_PL$confirmed)

predykcja2 = predict(model2,newdata = data.frame(date = c(64,65,66,67,68,69,70,71)))

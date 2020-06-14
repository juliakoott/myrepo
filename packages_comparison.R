getwd()

#####################################################################
#Instalacja paczek

install.packages("COVID19")
require("COVID19")

install.packages("coronavirus")
require("coronavirus")

require(dplyr)

#####################################################################
# Get top confirmed cases by state
coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total = sum(cases)) %>%
  arrange(-total) %>%
  head(20)

# Get the number of recovered cases in China by province
coronavirus %>%
  filter(type == "recovered", country == "China") %>%
  group_by(province) %>%
  summarise(total = sum(cases)) %>%
  arrange(-total)

#####################################################################
#Wczytanie danych dla Polski

covid_PL = covid19("PL")
coronavirus_PL = coronavirus %>%filter(country == "Poland")

covid_PL_dates = covid_PL[,2]
covid_PL_deaths = cbind(covid_PL_dates,covid_PL[,6])
covid_PL_infected = cbind(covid_PL_dates,covid_PL[,4])
covid_PL_recovered = cbind(covid_PL_dates,covid_PL[,5])

covid_PL_infected = data.matrix(covid_PL_infected)
covid_PL_deaths = data.matrix(covid_PL_deaths)
covid_PL_recovered = data.matrix(covid_PL_recovered)

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

coronavirus_PL_deaths_daily = coronavirus %>%filter(type == "death",country == "Poland")
coronavirus_PL_infected_daily = coronavirus %>%filter(type == "confirmed",country == "Poland")
coronavirus_PL_recovered_daily = coronavirus %>%filter(type == "recovered",country == "Poland")


infected_comp = c()
deaths_comp = c()
recovered_comp = c()

covid_PL_d_daily = data.matrix(covid_PL_deaths_daily[,2])
covid_PL_i_daily = data.matrix(covid_PL_infected_daily[,2])
covid_PL_r_daily = data.matrix(covid_PL_recovered_daily[,2])

coronavirus_PL_d_daily = data.matrix(coronavirus_PL_deaths_daily[,7])
coronavirus_PL_i_daily = data.matrix(coronavirus_PL_infected_daily[,7])
coronavirus_PL_r_daily = data.matrix(coronavirus_PL_recovered_daily[,7])

for(i in 1:(length(coronavirus_PL_d_daily))) {
  deaths_comp = c(deaths_comp,(coronavirus_PL_d_daily[i]-covid_PL_d_daily[i]))
  infected_comp = c(infected_comp,(coronavirus_PL_i_daily[i]-covid_PL_i_daily[i]))
  recovered_comp = c(recovered_comp,(coronavirus_PL_r_daily[i]-covid_PL_r_daily[i]))
}



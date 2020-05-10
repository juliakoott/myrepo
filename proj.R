# install.packages("devtools")
#install.packages("BiocManager")
library(BiocManager)
#BiocManager::install('fs')
#B#BiocManager::install('fansi')
#BiocManager::install('backports')
#BiocManager::install('plotly')
library(shiny)
library(plotly)
library(dplyr)
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)


##
data("coronavirus")
head(coronavirus)







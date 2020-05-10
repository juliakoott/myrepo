#setwd("D:/polsl/m1/wsp")

dane_2018<-read.csv("OGÓŁEM.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
K_2018<-read.csv("KOBIETY.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
M_2018<-read.csv("MĘŻCZYŹNI.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#przyczyny zgonów: "https://www.csioz.gov.pl/fileadmin/user_upload/Wytyczne/statystyka/icd10tomi_56a8f5a554a18.pdf"

#  B34.2 ->Zakażenie koronawirusowe o nieokreślonym umiejscowieniu 
# w danych z roku 2018 nie widnieją zgony spowodowane zakażeniem koronawirusami dla żadnej z grup wiekowych

dane_owd_death<-read.csv("annual-number-of-deaths-by-cause.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

dane_owd_covid<-read.csv("covid-confirmed-cases-since-100th-case.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")


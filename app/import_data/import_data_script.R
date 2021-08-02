
#Installation des packages
install.packages("rvest")
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("RTools")
install.packages("RSQLite")
install.packages("tidyr")

#Ajoute les librairies
library(rvest)
library(stringr)
library(readxl)
library(tidyverse)
library(DBI)
library(RSQLite)

#On fixe le chemin 
dir.create("C:/opendata")
setwd("C:/opendata")

#Web scrapping sur la page web data.gouv 
page_web <- read_html("https://www.data.gouv.fr/fr/posts/les-donnees-des-elections/")

#On recupere tous les liens xls et xlsx de la page web
fichiers.xls <- page_web %>% html_nodes("li > a ") %>% html_attr("href") %>% str_subset("\\.xls")
liens_xls <- data.frame(fichiers.xls)
colnames(liens_xls) <- "Fichiers XLS du site Data.gouv"
View(liens_xls)

#On va creer un fichier csv qui rassemble tous les liens xls de la page web.
#Je conseil de l'ouvrir avec NotePad++ qui permet d'avoir les liens clickables.
write.csv2(liens_xls, file="C:/Users/lebriat/Downloads/liens_xls_page_web", row.names = FALSE)

#On recupere tous les liens csv de la page web 
fichiers.csv <- page_web %>% html_nodes("li > a ") %>% html_attr("href") %>% str_subset("\\.csv")
liens_csv <- data.frame(fichiers.csv)
colnames(liens_csv) <- "Fichiers CSV du site Data.gouv"
View(liens_csv)

#On va creer un fichier csv qui rassemble tous les liens xls et xlsx de la page web.
#Je conseil de l'ouvrir avec NotePad++ qui permet d'avoir les liens clickables.
write.csv2(liens_csv, file="C:/Users/lebriat/Downloads/liens_csv_page_web", row.names = FALSE)

#Telecharger un x nombre de fichier
for(i in 9:11){
  #On telecharge les fichiers csv en les nommant diff?rement 
	download.file(liens_csv[i,], destfile = paste('elections_legislatives',i,'.csv',sep=''))
} 
  #Telecharger un x nombre de fichier
  for(j in 158:158){
    #On telecharge les fichiers xls en les nommant diff?rement 
    download.file(liens_xls[j,], destfile = paste('elections_presidentielles',j,'.xls'),mode = "wb", method="libcurl")
  }

#Ouvrir les fichiers recement telecharges
files_csv <- sort(list.files(pattern=".csv"))
data_list_csv <- lapply(files_csv,read.csv,encoding = "UTF-8")

#On automatise le traitement des donn?es
#On garde que les colonnes int?ressantes et utiles pour les repr?sentations graphiques futures. 
i<-1
for (i in 1:length(data_list_csv)){
  data_list_csv[[i]] <- data_list_csv[[i]][-c(1,1),-c(8:24)]
  i+1
  data_list_csv[[i]]<-na.omit(data_list_csv[[i]])
}
View(data_list_csv)


#Connexion a la base de donnees 
connexion_SQLite <- DBI::dbConnect(RSQLite::SQLite(), dbname = "elections_legistatives.sqlite")
connexion_SQLite

#On tranforme les donnees data_list_csv en dataframe
data_legis <- as.data.frame(data_list_csv[[1]])
View(data_legis)

#On va ?crire les donnees de data_legis dans une table "elections_legislatives
dbWriteTable(connexion_SQLite,"elections_legislatives",data_legis)

#On va lire la table 
dbReadTable(connexion_SQLite, "elections_legislatives")

#On va lister les tables qui sont contenues dans la base de donn?es 
dbListTables(connexion_SQLite)

#Cette commande va permettre de lire la table "elections_legislatives"
dbReadTable(connexion_SQLite,"elections_legislatives",max=getOption("max.print"))

#Requ?tes SQL pour recuperer les donnees de la table
rs <- dbSendQuery(connexion_SQLite, "select * from elections_legislatives")
d1 <- fetch(rs, n = 790)      # extract data in chunks of 10 rows
d1

#On peut faire ensuite des graphiques
#On peut voir les votants en fonction des inscrits
plot(d1$Votants~d1$Inscrits)

#Calculer le taux de participation 
taux_participation<-round(d1$Votants/d1$Inscrits*100,2)
taux_participation

#Affichage des resulats du taux de participation en fonction du nombre d'inscrit
plot(taux_participation~d1$Inscrits)





# Installation des packages

install.packages("readxl")
install.packages("psych")

# working  directory
getwd()

# changer le répertoire de travail
setwd('C:/Users/UNCHK/Documents')

# chargement des packages
library(readxl)
library(psych)
# importation des données
Base_Marketing <- read_excel("Base_de_donnees_marketing.xlsx")

# copie des données
dataset <- Base_Marketing

# visualition des données
View(dataset)
head(dataset)

attach(dataset)

# Preprocessing
# unidimensionnel analyse

str(dataset) # structuration for the data
summary(dataset)
describe(dataset) 

# visualisation des données manquantes

sum(is.na(dataset))

# visualisation des données

par(mfrow = c(2,2), mar = c(4,4,2,1))


hist(dataset$sales, prob = TRUE, nclass = 30, col= "lightblue",
	main = "Distribution des ventes", xlab = "Sales", ylab = "Densité" ) 
lines(density(dataset$sales), col = "red", lwd = 2)

hist(dataset$youtube, prob = TRUE, nclass = 50, col= "lightyellow",main = "Distribution youtube",xlab = "Youtube", ylab = "Densité" ) 
lines(density(dataset$youtube), col = "red", lwd = 2)

hist(dataset$facebook, prob = TRUE, nclass = 50, col= "lightgreen",main = "Distribution Facebook",xlab = "Facebook", ylab = "Densité" ) 
lines(density(dataset$facebook), col = "red", lwd = 2)

hist(dataset$newspaper, prob = TRUE, nclass = 50, col= "lightpink",main = "Distribution Newspaper",xlab = "Newspaper", ylab = "Densité" ) 
lines(density(dataset$newspaper), col = "red", lwd = 2)


# Division de notre base de données

set.seed(101)
n_total <- nrow(dataset)
taille_train <- floor(0.80 * n_total)
indices_train = sample_int(n_total, size = taille_train, replace = FALSE)
train <- marketing[indices_train,]
test <- marketing[-indices_train,]


# création du modèle
Modele <- ln(dataset$sales-dataset$youtube+dataset$facebook+dataset$newpappers, data=train)
summary(Modele)









nom du groupe
sujet 










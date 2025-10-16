# ===================================================================================
# Analyse de données avec R
# Population: 60 salariés
# Caractère étudié: Salaire 
# Nature: quantitative continue
# Objectifs:
#		1- Déterminer la moyenne arithmétique et la médiane de la distribution.
#		   Interpréter.
#		2- Calculer l'écart-type, le coefficient de variation et l'intervalle
#		   interquartile.
#           3- Que pensez-vous de l'asymétrie et de l'aplatissement de la 
#		   distribution?
#		4- Déterminer la médiale de la distribution. Interpréter.
#		5- La distribution des salaires est-elle égalitaire?
#		   
# ==================================================================================

#-----------------------------
# Chargement des bibliothèques
#-----------------------------

library(dplyr)
library (ggplot2)
library(intervals)

#--------------------
# Données d'entrées
#--------------------

Modalites <- c("[2;3[","[3;4[","[4;6[","[6;8[","[8;12[")
Effectifs <- c(15,12,21,9,3)

#---------------------
# Calculs statistiques
#---------------------

Bornes_inf <- c(2,3,4,6,8)
Bornes_sup <- c(Bornes_inf,12)
Bornes_sup <- Bornes_sup[-1]
Centres <- (Bornes_inf + Bornes_sup) / 2
Population <- get_population(Effectifs)
Moyenne <- get_moyenne(Centres,Effectifs,Population)
Frequences <- get_frequences_rel(Effectifs,Population)
FrequencesCC <- get_cumul_c(Frequences)
Quartiles <- get_quartiles(Bornes_inf,Bornes_sup,FrequencesCC)  # valeur quartiles : 2.555556 5.400000 5.400000
					#(La fonction qui calcul les quartiles ne marche pas quand 0.25, 0.5, 0.75 n'ont pas une valeur qui les précéde et une autre qui les suit)
Ecart_Type <- sqrt(get_var(Centres,Effectifs,Moyenne,Population))
Coefficient_variation <- Ecart_Type/Moyenne
IIQ <- Intervals(matrix(c(Quartiles[1],Quartiles[3]), ncol = 2),closed = c(TRUE,TRUE))
Coefficient_Yule <- get_coef_yule(Quartiles)
#----------------------------------------------------------------------
# Affichage des résultats et du tableau statistique et interprétations
#----------------------------------------------------------------------

cat("Le salaire moyen annuel de la distribution est de ",Moyenne*1000,"F CFA.\n")
cat("50% des salariés ont un salaire annuel inférieur ou égal à ",round(Quartiles[2]*1000,2),"F CFA.\n")
cat("L'intervalle interquartile est:", IIQ , ".\n")





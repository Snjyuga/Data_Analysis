# ===================================================================================
# Analyse de données avec R
# Population: Superficies (hectare)
# Caractère étudié: les distributions des exploitations agricoles.
# Nature: quantitative continue
# Objectifs:
#		1- Tracer l'histogramme de la distribution de la superficie pour les
#		   deux années.
#		2- Calculer pour chaque distribution, les paramètres de tendance
#		   centrale et les paramètres de dispersion.
#           3- Comparer les deux distributions.
#		4- Calculer la médiale des deux distributions.Interpréter.
#		5- Evaluer la concentration.
# ==================================================================================

#-----------------------------
# Chargement des bibliothèques
#-----------------------------

library(ggplot2)
library(dplyr)

#--------------------
# Données d'entrées
#--------------------

Superficies <- c("[5;10]","[10;20]","[20;30]","[30;40]","[40;60]";"[60;80]")
Frequences_1990 <- c(14.5,16.5,20.2,25.1,18.5,5.2)
Frequences_2000 <- c(7.2,12.2,18.6,28.2,26.4,7.4)

#-----------------------
# Calculs statistiques
#-----------------------

Bornes_inf <- c(5,10,20,30,40,60)
Bornes_sup <- c(Bornes_inf,80)
Bornes_sup <- Bornes_sup[-1]
Centres <- (Bornes_inf + Bornes_sup) / 2
Population1990 <- get_population(Frequences1990)
Population2000 <- get_population(Frequences2000)
EffectifsCC1990 <- get_cumul_c(Frequences1990)
EffectifsCC2000 <- get_cumul_c(Frequences2000)
Frequences1990 <- get_frequences_rel(Frequences_1990,Population1990) 
FrequencesCC1990 <- get_cumul_c(Frequences) 






# ===================================================================================
# Analyse de données avec R
# Population: habitant d'une commune
# Caractère étudié: le montant annuel des impots locaux
# Nature: quantitative continue
# Objectifs:
#		1- Quelle est la population étudiée? Quel est le caractère observé?
#		   Quelle est sa nature?
#		2- Construire l'histogramme de cette distribution et le polygone
#		   des fréquences.
#           3- Construire la courbe des fréquences cumulées croissantes.
#		4- Déterminer la classe modale, la médiane et les quartiles Q1 et Q3.
#		5- Calculer la moyenne arithmétique de cette série, l'écart-interquar-
#		   tile, l'intervalle interquartile,la variance et le coefficient de variation.
#		6- On veut étudié la concentration des impots locaux:
#		a- Définir et calculer l'écart de concentration.
#		b- Construire la courbe de concentration de Gini et calculer l'indice
#		   de Gini.Conclusion.
#		c- Quelle est l'influence sur l'indice de Gini d'une augmentation de 
#		   10% des impots?
# ==================================================================================

#-----------------------------
# Chargement des bibliothèques
#-----------------------------

library(intervals)
library(ggplot2)
library(dplyr)

#--------------------
# Données d'entrées
#--------------------

Effectifs <- c(1,7,11,8,12,15,19,16,8,3) 
Classes <- c("[2;4[","[4;6[","[6;8[","[8;9[","[9;10[","[10;12[","[12;16[","[16;20[","[20;40[","[40;80[")

#---------------------
# Calculs statistiques
#---------------------

Bornes_inf <- c(2,4,6,8,9,10,12,16,20,40)
Bornes_sup <- c(Bornes_inf,80)
Bornes_sup <- Bornes_sup[-1]
Centres <- (Bornes_inf + Bornes_sup) / 2
Population <- get_population(Effectifs)
EffectifsCC <- get_cumul_c(Effectifs)
Frequences <- get_frequences_rel(Effectifs,Population) 
FrequencesCC <- get_cumul_c(Frequences) 
Classe_modale <- get_classe_modale(Classes,Tableau$densites)
Quartiles <- get_quartiles(Bornes_inf,Bornes_sup,FrequencesCC)
Moyenne <- get_moyenne(Centres,Effectifs,Population)
EIQ <- Quartiles[3] - Quartiles[1]
IIQ <- Intervals(c(Quartiles[1],round(Quartiles[3],1)))
Variance <- get_var(Centres,Effectifs,Moyenne,Population)
Coefficient_Variation <- Variance / Moyenne
Deciles <- get_deciles(Bornes_inf,Bornes_sup,FrequencesCC)
Ecart_De_Concentration <- Deciles[9] - Deciles[1]
Rapport_De_Concentration <- Deciles[9] / Deciles[1]
Masse_Classe <- Effectifs * Centres
Masse_Totale <- sum(Masse_Classe)  
Frequence_Masse <- Masse_Classe / Masse_Totale
Frequence_cumulee_masse <- cumsum(Frequence_Masse)
Aire <- sum((Frequence_cumulee_masse[-1] + Frequence_cumulee_masse[-length(Frequence_cumulee_masse)]) * diff(FrequencesCC) / 2)
Gini <- 1 - (2 * Aire)

BI2 <- c(2,4,6,8,9,10,12,16,20,40)
BI2 <- BI2 + 10 
BS2 <- c(4,6,8,9,10,12,16,20,40,80)
BS2 <- BS2 + 10
Bornes2 <- c(BI2,BS2)
Valeurs2 <- matrix(Bornes2,ncol = 2,nrow = 10,byrow = FALSE) 
Modalites2 <- Intervals(Valeurs2,closed = c(TRUE,FALSE),type = "Z")
Centres2 <- (BI2 + BS2) / 2
Masse_Classe2 <- Effectifs * Centres2
Masse_Totale2 <- sum(Masse_Classe2)
Frequence_Masse2 <- Masse_Classe2 / Masse_Totale2
Frequence_cumulee_masse2 <- cumsum(Frequence_Masse2)
Aire2 <- sum((Frequence_cumulee_masse2[-1] + Frequence_cumulee_masse2[-length(Frequence_cumulee_masse2)]) * diff(FrequencesCC) / 2)
Gini2 <- 1 - (2 * Aire2)
Différence <- Gini - Gini2

#----------------------------------------------------------------------
# Affichage des résultats et du tableau statistique et interprétations
#----------------------------------------------------------------------

Tableau <- tibble::tibble(
	classes = Classes,
	centres = Centres,
	bornes_inf = Bornes_inf,
	bornes_sup = Bornes_sup,
	effectifs = Effectifs,
	effectifs_cumules = EffectifsCC,
	frequences = round(Frequences, 2),
	frequences_cumulees = round(FrequencesCC, 2)
)
Tableau <- Tableau %>%
	mutate(
	amplitudes = bornes_sup - bornes_inf,
	densites = frequences / amplitudes
	)
cat("La population des habitant de cette commune s'éléve à ", Population,"personnes.\n")
cat("La classe modale de la série est  égal à:", Classe_modale,". \n")
View(Tableau)
cat("25 % des habitants de cette commune paient un impot égal à ", quartile,"%\n.")
cat("L’écart de concentration est une mesure statistique de dispersion qui permet 
d’évaluer le degré d’inégalité dans la répartition d’une grandeur 
(comme les revenus, les salaires, ou ici les impôts locaux) au sein d’une population")
cat("L'indice de Gini de ",Gini'" calculé pour la répartition des impôts locaux indique 
un degré d'inégalité modéré au sein de la population communale.
 Concrètement , cela signifie que la répartition de la charge fiscale n'est pas parfaitement équilibrée : 
certains habitants contribuent davantage aux impôts locaux que d'autres, mais les écarts restent limités . 
En d'autres termes, la charge fiscale locale est modérément concentrée dans la population : 
on n'observe pas une situation extrêmement inégalitaire où une petite minorité assumerait 
l'essentiel des impôts, bien que la répartition ne soit pas totalement égalitaire.\n")
cat("Une baisse de ",round(Gini,2)," à", round(Gini2,2)," signifie un renforcement très net de l'égalité 
dans la répartition de la charge fiscale : le poids de l'impôt est partagé 
de façon beaucoup plus homogène entre les contribuables.\n")
#--------------
# Visualitions
#--------------

Tableau$classes <- factor(
	Classes,
	levels = Classes,
	ordered = TRUE
)

ggplot(Tableau, aes(xmin = bornes_inf,xmax = bornes_sup,
			  ymin = 0, ymax = densites)) + 
	geom_rect(fill = "skyblue", color = "red") +
	labs(title = "Histogramme des fréquences (amplitudes inéales)",
	x = "Classes",
	y = "Densité(Fréquences / Amplitudes)")

ggplot(Tableau,aes(x = centres, y = frequences)) +
	geom_point(color = "red", size = 2) +
	geom_line(color = "red", linewidth = 1) +
	labs(title= "Polygone des fréquences",
		x = "Centres des classes",
		y = "Fréquences")


ggplot(Tableau, aes(x = centres, y = frequences_cumulees)) +
  geom_step() +
  geom_point(color = "blue") +
  labs(
    title = "Courbe des fréquences cumulées croissantes",
    x = "Centres de classes",
    y = "Fréquences cumulées"
  ) +
  theme_minimal()


Lorentz <- data.frame(X = round(FrequencesCC,2),Y = round(Frequence_cumulee_masse,2))

ggplot(Lorentz, aes(x = X, y = Y)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Courbe de concentration (Lorentz)",
    x = "Fréquences cumulées des effectifs",
    y = "Fréquences cumulées des masses"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 13)


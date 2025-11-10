#================================================================================
#  Analyse de données avec R
#  Population: Entreprises industrielles sénégalaises exportatrices de 20
#  		   employés et plus.
#  Caractère étudié: Chiffre d'affaires (en milliers de F CFA)
#  Nature: Quantitative continue
#  Objectifs:
#		1- Calculer le mode, la médiane, et la moyenne arithmétique de cette
#		   distribution.
#		2- Calculer l'écart-type, le coefficient de variation et l'intervalle
#		   interquartile.
#		3- Calculer le coefficient d'asymétrie de Yule. Interpréter.
#		4- Calculer le coefficient d'applatissement de Fisher. Interpréter.
#		5- Calculer la différence médiale-médiane et évaluer la concentration
#		6- Tracer la courbe de Lorenz et calculer l'indice de concentration
#		   de Gini, interpréter.
#=================================================================================

#------------------------------
# Chargement des bibliothèques
#------------------------------

library(dplyr)
library (ggplot2)
library(intervals)

#------------------
# Données d'entrées
#------------------

Modalites <- c("[0;200[","[200;400[","[400;1000[","[1000;2000[","[2000;3000[")
Frequences <- c(0.6,0.2,0.12,0.07,0.01)

#---------------------
# Calculs statistiques
#---------------------

Bornes_inf <- c(0,200,400,1000,2000)
Bornes_sup <- c(Bornes_inf,3000)
Bornes_sup <- Bornes_sup[-1]
Centres <- (Bornes_inf + Bornes_sup) / 2
Population <- get_population(Frequences)
Amplitudes <- Bornes_sup - Bornes_inf
Densites <- Frequences / Amplitudes
which(Densites == max(Densites))
Mode <- Bornes_inf[1]+Amplitudes[1]*Densites[1]/ Densites[1] - Densites[2] #ma fonction actuelle plante lorsqu'il n'y a pas de valeur et après la classe modale
Moyenne <- get_moyenne(Centres,Frequences,Population)
FrequencesCC <- get_cumul_c(Frequences)
Quartiles <- c(150,150,325)
Ecart_Type <- sqrt(get_var(Centres,Frequences,Moyenne,Population))
Coefficient_variation <- Ecart_Type/Moyenne
IIQ <- Intervals(matrix(c(Quartiles[1],Quartiles[3]), ncol = 2),closed = c(TRUE,TRUE))
Coefficient_Yule <- get_coef_yule(Quartiles)
Coefficient_Fisher <- ((sum(Frequences*(Centres - Moyenne)^4)) / (Ecart_Type)^4) - 3
Masse_Classe <- Frequences * Centres
Masse_cumulee_c <- get_cumul_c(Masse_Classe)
Masse_Totale <- get_population(Masse_Classe)  
Frequence_Masse <- Masse_Classe / Masse_Totale
Frequence_cumulee_masse <- get_cumul_c(Frequence_Masse)
A <- Bornes_sup[3] - Bornes_inf[3]
Mediale <- Bornes_inf[3] + A*((Masse_Totale/2)-Masse_cumulee_c[2])/ (Masse_cumulee_c[3] - Masse_cumulee_c[2])
Mediale_Mediane <- Mediale - Quartiles[2]
Aire <- sum((Frequence_cumulee_masse[-1] + Frequence_cumulee_masse[-length(Frequence_cumulee_masse)]) * diff(FrequencesCC) / 2)
Gini <- 1 - (2 * Aire)

#-------------------------------------------------------
# Affichage des résultats, du tableau et interprétations
#-------------------------------------------------------

Tableau <- tibble::tibble(
	Modalites,
	Masse_cumulee_c,
	Bornes_inf,
	Bornes_sup,
	Centres,
	Frequences,
	FrequencesCC,
	Amplitudes,
	Densites
)
View(Tableau)
cat("Avec un coefficient de Fisher égal à",Coefficient_Fisher,",la distribution possède une forme pointue au niveau de la moyenne avec des extrémités plus longues et étendues. On parle de distribution leptokurtique.\n")
cat("Un grand écart de concentration, comme c'est le cas ici,",Mediale_Mediane," indique une forte concentration. Cela signifie qu'une petite partie des entreprises détient une part disproportionnée et très importante de la "masse" totale.\n")
cat("Gini = ",Gini," > 0,5 : indique une forte concentration.L'indice de Gini vient appuyer l'interprètation de la médiale.\n")

#---------------
# Visualisations
#---------------

Tableau$Modalites <- factor(
	Modalites,
	levels = Modalites,
	ordered = TRUE
)
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















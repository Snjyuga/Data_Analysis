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
i <- 1
while( i <= length(Bornes_inf)){
	if(FrequencesCC[i] >= 0.5){
		Fi <- FrequencesCC[i]
		Fi_1 <- FrequencesCC[i - 1]
		Fii <- FrequencesCC[i +1]
		xi <- Bornes_inf[i]
		xii <- Bornes_sup[i]
		break
	}
	i <- i + 1
} 
Mediane <- xi + (xii - xi)*(Fi - Fi_1) / (Fii - Fi_1)
#(La fonction qui calcul les quartiles ne marche pas quand 0.25, 0.5, 0.75 n'ont pas une valeur qui les précéde et une autre qui les suit)

#----------------------------------------------------------------------
# Affichage des résultats et du tableau statistique et interprétations
#----------------------------------------------------------------------

cat("Le salaire moyen annuel de la distribution est de ",Moyenne*1000,"F CFA.\n")
cat("50% des salariés ont un salaire annuel inférieur ou égal à ",Mediane*1000,"F CFA.\n")



Mediane <- get_quartiles(Bornes_inf,Bornes_sup,FrequencesCC)

Tableau$Modalites <- factor(
	Modalites,
	levels = Modalites,
	ordered = TRUE
)

Tableau <- tibble::tibble(
	Modalites = Modalites,
	Bornes_inf = Bornes_inf,
	Bornes_sup = Bornes_sup,
	Centres = Centres,
	Frequences = Frequences,
	FrequencesCC = FrequencesCC
)
Tableau <- Tableau %>%
	mutate(
	Amplitudes = Bornes_sup - Bornes_inf,
	Densites = Frequences / Amplitudes
	)
View(Tableau)

ggplot(Tableau, aes(xmin = Bornes_inf,xmax = Bornes_sup,
			  ymin = 0, ymax = Densites)) + 
	geom_rect(fill = "skyblue", color = "red") +
	labs(title = "Histogramme des fréquences (amplitudes inégales)",
	x = "Classes",
	y = "Densité(Fréquences / Amplitudes)")
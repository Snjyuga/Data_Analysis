# ========================================================================
# Analyse de données avec R
# Population: Salariés d'une entreprise données
# Caractère étudié: Prime de fin d'année attribuées
# Nature: Quantitative continue
# Objectifs:
#		1- Quelle est la population étudiée? Préciser le caractère étudié
#		   ainsi que sa nature.
#		2- Pourquoi a t-on regroupé les primes en classes?
#           3- Déterminer la moyenne et l'écart-type.
#		4- Tracer la courbe des fréquences cumulées croissantes.
#		5- Déterminer graphiquement la médiane. Interpréter cette valeur.
# =========================================================================

#---------------------------
# Chargement des packages
#-----------------------
library(ggplot2)
library(tibble)
library(dplyr)
library(testthat)
#--------------------
# Données d'entrées
#--------------------

Effectifs <- c(62,92,122,89,35) 
Classes <- c("[0;4[","[4;8[","[8;12[","[12;16[","[16;20[")
Bornes <- seq(from = 0,to = 20, by = 4)
Bornes_inf <- head(Bornes, -1)
Bornes_sup <- tail(Bornes, -1)

#----------------------
# Calculs statistiques
#-----------------------

Population <- get_population(Effectifs)
EffectifsCC <- get_cumul_c(Effectifs)
Frequences <- get_frequences_rel(Effectifs,Population) 
FrequencesCC <- get_cumul_c(Frequences) 
Centres <- (Bornes_inf + Bornes_sup) / 2
Moyenne <- get_moyenne(Tableau$centres,Tableau$Effectifs)
Ecart_type <- sqrt(get_var(Tableau$centres,Tableau$effectifs,Moyenne,Population))
Mediane <- get_mediane(Bornes_inf,Bornes_sup,FrequencesCC)

#--------------------------------------------------------------------
# Affichage des résultats et du tableau statistique et interprétation
#--------------------------------------------------------------------

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
cat("La population des salariés de l'entreprise est de: ", Population,".\n")
View(Tableau)
cat("Les salariés ont en moyenne une prime de fin d'année égale à : ", Moyenne, "%.\n")
cat("L'écart-type de la série est égal à:", Ecart_type, ".\n")
x <- 10
cat("La médiane graphique est proche de",x, ", tandis que la médiane calculée par interpolation linéaire est de" ,Mediane,".
L’écart est inférieur à une demi-largeur de classe, donc il peut être considéré comme négligeable.
Cela illustre que la lecture graphique est une bonne approximation, mais que l’interpolation fournit une valeur plus précise.
La moitié des salariés de cette entreprise ont une prime de fin d'année inférieur ou égale à ", Mediane, "%.\n")

#--------------
# Visualitions
#--------------

Tableau$classes <- factor(
	Classes,
	levels = Classes,
	ordered = TRUE
)

Courbe_cumul_croissante <- ggplot(Tableau, aes(x = Centres, y = FrequencesCC)) +
  geom_step() +
  geom_point(color = "blue") +
  labs(
    title = "Courbe des fréquences cumulées croissantes",
    x = "Centres de classes",
    y = "Fréquences cumulées"
  ) +
  theme_minimal()


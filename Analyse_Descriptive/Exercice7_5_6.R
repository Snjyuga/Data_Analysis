# =======================================================================================
# Analyse de données avec R
# Population: Employés d'une entreprise données
# Caractère étudié: Age et sexe
# Nature: Quantitative continue et qualitative nominal
# Objectifs:
#		1- Calculer la moyenne d'age par sexe.
#		2- En déduire la moyenne d'age de la population totale.
#           3- Entre ces populations, laquelle est la plus homogène relativement à l'age.
# =======================================================================================

#--------------------
# Données d'entrées
#--------------------

Classes <- c("[20;30[","[30;40[","[40;50[","[50;60[")
Bornes <- seq(from = 20, to = 60, by = 10)
Bornes_inf <- head(Bornes, -1)
Bornes_sup <- tail(Bornes, -1)
Centres <- (Bornes_inf + Bornes_sup) / 2
Effectifs_Hommes <- c(75,86,82,57)
Effectifs_Femmes <- c(85,76,60,29)

#----------------------
# Calculs statistiques
#----------------------

Population_Hommes <- get_population(Effectifs_Hommes)
Population_Femmes <- get_population(Effectifs_Femmes)
Population_Par_Sexe <- c(Population_Hommes,Population_Femmes)
Population <- get_population(Population_Par_Sexe)
Moyenne_Hommes <- get_moyenne(Centres,Effectifs_Hommes)
Moyenne_Femmes <- get_moyenne(Centres,Effectifs_Femmes)
Moyenne_Par_Sexe <- c(Moyenne_Hommes,Moyenne_Femmes)
Moyenne <- sum(Moyenne_Par_Sexe) / length(Population_Par_Sexe)
Variance_Hommes <- get_var(Centres,Effectifs_Hommes,Population_Hommes)
Variance_Femmes <- get_var(Centres,Effectifs_Femmes,Population_Femmes)
Coefficient_Var_Hommes <- sqrt(Variance_Hommes) / Moyenne_Hommes
Coefficient_Var_Femmes <- sqrt(Variance_Femmes) / Moyenne_Femmes

#--------------------------------------------------------------------
# Affichage des résultats et interprétation
#--------------------------------------------------------------------

cat("L'age moyen des hommes de cette entreprise est de ",round(Moyenne_Hommes,2),"ans.\n")
cat("L'age moyen des femmes de cette entreprise est de ",round(Moyenne_Femmes,2),"ans.\n")
cat("L'age moyen tout sexe confondu dans cette entreprise est de ",round(Moyenne,2),"ans.\n")
cat("La population des femmes est la plus homogène car son coefficient de variation est inférieure à celle des hommes.\n")

#================================================================================================
# Analyse de données avec R
# Population: Employés d'une entreprise données
# Caractère étudié: Salaires
# Nature:quantitatif continue
# Objectifs:
#		1-Calculer la proportion d'employés qui perçoivent au moins 400.000 FCFA.
#		2-Calculer les quartiles de cette série. Interpréter ces différentes valeurs.
#		3-Réaliser la boite à moustaches et la courbe des fréquences cumulées décroissantes.
#		4-Calculer la moyenne et la variance de cette série statistique.
#		5-Calculer le coefficient de variation. Que peut-on en déduire?
#================================================================================================

#-------------------------
#Chargement des librairies
#-------------------------

library(ggplot2)
library(tibble)

#-------------------
# Données d'entrées
#--------------------

Effectifs <- c(36,62,50,28,20,4)
Classes <- c("[20;25[","[25;30[","[30;35[","[35;40[","[40;45[","[45;50[")
Bornes <- seq(from = 20,to = 50, by = 5)
Bornes_inf <- head(Bornes, -1)
Bornes_sup <- tail(Bornes, -1)

#----------------------
# Calculs statistiques
#----------------------

Population <- get_population(Effectifs)
EffectifsCC <- get_cumul_c(Effectifs)
Frequences <- get_frequences_rel(Effectifs,Population) 
FrequencesCC <- get_cumul_c(Frequences)
FrequencesCD <- rev(FrequencesCC)
Centres <- (Bornes_inf + Bornes_sup) / 2
Proportion <- get_proportion(Bornes_inf,Frequences)
quartile1 <- get_mediane(Bornes_inf,Bornes_sup,FrequencesCC) #26.12903
Mediane <- get_mediane(Bornes_inf,Bornes_sup,FrequencesCC)   # 30.2
quartile3 <- get_mediane(Bornes_inf,Bornes_sup,FrequencesCC)  # 35.35714
Moyenne <- get_moyenne(Centres,Effectifs,Population)
Variance <- get_var(Centres,Effectifs,Moyenne,Population)
CV <- get_variation(Variance,Moyenne)
#--------------------------------------------------------------------
# Affichage des résultats et du tableau statistique et interprétation
#--------------------------------------------------------------------

cat("La population des salariés de l'entreprise est de: ", Population,".\n")
Tableau <- tibble::tibble(
	classes = Classes,
	centres = Centres,
	bornes_inf = Bornes_inf,
	bornes_sup = Bornes_sup,
	effectifs = Effectifs,
	effectifs_cumules = EffectifsCC,
	frequences = round(Frequences, 2),
	frequences_cumulees = round(FrequencesCC, 2),
	frequences_cumul_decroi = round(FrequencesCD,2)
)
View(Tableau)
cat("La proportion d'employés qui perçoivent au moins 400 000 F CFA est de:",Proportion,"%.\n")
cat("25 % des employés de cette entreprise perçoivent un salaire inférieure ou égal à",quartile1*10000,"F CFA.\n")
cat("50 % des employés de cette entreprise perçoivent un salaire inférieure ou égal à",Mediane*10000,"F CFA.\n")
cat("75 % des employés de cette entreprise perçoivent un salaire inférieure ou égal à",quartile3*10000,"F CFA.\n")
cat("Le salaire moyen perçu par les salariés dans cette entreprise s'éléve à ",Moyenne *10000,"F CFA.\n" )
cat("La variance de la série est égale à ",Variance,".\n")
CV
#-------------
Visualisation
#-------------

resume <- tibble(
  variable = "X",   # une catégorie factice (utile si une seule boîte)
  ymin   = min(Bornes_inf),
  lower  = quartile1,
  middle = Mediane,
  upper  = quartile3,
  ymax   = max(Bornes_sup)
)

Boite_a_moustache <- ggplot(resume, aes(x = variable, 
                   ymin = ymin, lower = lower, middle = middle, 
                   upper = upper, ymax = ymax)) +
  geom_boxplot(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Boîte à moustaches (valeurs calculées)",
       x = "", y = "Valeurs") +
  theme_minimal()

donnees <- tibble(
  Bornes_sup = Bornes_sup,
  Freq_cum_dec = FrequencesCD
)

Courbes_cumulative <- ggplot(donnees, aes(x = Bornes_sup, y = Freq_cum_dec)) +
  geom_step(direction = "vh", color = "red", size = 1) +
  labs(title = "Courbe des fréquences cumulées décroissantes",
       x = "Bornes supérieures",
       y = "Fréquences cumulées décroissantes") +
  scale_y_continuous(labels = scales::percent) +  # pour afficher en %
  theme_minimal()
Courbes_cumulative


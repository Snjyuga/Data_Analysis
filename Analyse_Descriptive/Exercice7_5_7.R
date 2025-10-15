#====================================================================================
# Analyse de données avec R
# Population: Utilisateurs du site ayant attribués une note au film "Café Société".
# Caractère étudié: Avis sur des films
# Modalité: Niveau de satisfaction entre 0 et 5 étoiles
# Nature: qualitatif ordinale
# Objectifs: 
		1- Identifier la population,le caractère étudié et sa nature.
		2- Calculer la proportion d'avis notés.
		a-d'au plus 2 étoiles;b- d'au moins 3 étoiles; c- entre 1 et 4 étoiles
		bornes exclues.
		3- Calculer les quartiles de cette série statistique.Commenter la 
		symétrie.
		4- Calculer la moyenne et la variance de cette série. Commenter la 
		dispersion.
#=====================================================================================


#-------------------------------
Redaction du tableau statistique
#-------------------------------


NombreD_etoiles <- seq(from= 0,  to = 5 , by = 1 )	# Création du vecteur NombreD_etoiles


NombreD_avis <- c(59 , 178 , 505 , 1040 , 980 , 238 )	# Vecteur NombreD_avis


Population <- get_population(NombreD_avis)	# Calcul de la population


EffectifsCC <- get_effectifs_cumulees(NombreD_avis)	# Création du vecteur EffectifsCC


Frequences <- get_frequences_rel(NombreD_avis , Population)	# Création du vecteur Fréquences


FrequencesCC <- get_frequences_cc(Frequences)	# Création du vecteur FrequencesCC


FrequencesCD <- rev(FrequencesCC)	# Création du vecteur FrequencesCD


Tableau <- data.frame(NombreD_etoiles , NombreD_avis , EffectifsCC , Frequences , FrequencesCC , FrequencesCD )
View(Tableau)
#--------------------------------
# 2-La proportion d'avis notés
#--------------------------------

Proportion_a <- get_proportion_a(NombreD_etoiles,FrequencesCC)
Proportion_b <- get_proportion_b(NombreD_etoiles,Frequences)
Proportion_c <- get_proportion_c(NombreD_etoiles,Frequences)

#------------------------
# 3-Commenter la Symétrie
#-------------------------

Quartiles <- get_quartiles(NombreD_etoiles,EffectifsCC,Population)
Yule <- get_coef_yule(Quartiles)

#---------------------------------------------------------------
# 4- Calculer la moyenne, la variance et commenter la dispersion
#----------------------------------------------------------------

Moyenne <- get_mean(NombreD_etoiles,NombreD_avis, Population)


Variance <- get_var(NombreD_etoiles,NombreD_avis,Moyenne,Population)


CV <- get_variation(Variance,Moyenne)
CV

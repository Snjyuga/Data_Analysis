#===========================================================================
# Analyse de données avec R
# Population: 1580 ménages
# Caractère étudié:Nombre d'enfants
# Nature:quantitatif discret
# Objectifs:
#		Calculer la proportion de ménages ayant moins de quatre enfants
#		Déterminer le mode et l'étendue de la série
#		Calculer le nombre moyen d'enfants par ménage
#===========================================================================


#-------------------------------------------------------------------
# Tableau de Données:Nombre_Enfants,Effectifs ,EffectifsCC,Frequences,FrequencesCC
#-------------------------------------------------------------------

Nombre_Enfants<-seq(from=0, to= 8) # vecteur nombre d'enfants qui correspond aux modalités

Effectifs<-c(286,380,416,258,112,62,47,12,7) # vecteur éffectifs qui correspond aux éffectifs des modalités

i<-1
Population<-0
for (i in 1:9){Population<-Population + Effectifs[i]} # calcul de la population

EffectifsCC <- rep(0,time = 9)  # vecteur EffectifsCC qui correspond aux Effectifs cumulées croissantes
EffectifsCC[1] <- Effectifs[1]
i<-2
for (i in 2:9) {EffectifsCC[i] <- EffectifsCC[i-1]+Effectifs[i]}

i <- 1
Frequences <-rep(0,time = 9) # vecteur Frequences qui correspond aux Frequences relatives
for (i in 1:9) {Frequences[i] <- round(Effectifs[i]/Population, 4)}

FrequencesCC <- rep(0,time = 9)  # vecteur FrequencesCC qui correspond aux fréquences cumulées croissantes
FrequencesCC[1] <- Frequences[1]
i<-2
for (i in 2:9) {FrequencesCC[i] <- FrequencesCC[i-1]+Frequences[i]}

Tableau <- data.frame(Nombre_Enfants, Effectifs,EffectifsCC,Frequences,FrequencesCC)

#------------------------------------------------------------------
# 1-Calculer la proportion de ménages ayant moins de quatre enfants
#------------------------------------------------------------------

cat("La proportion de ménages ayant moins de quatre enfants est de:",FrequencesCC[4]*100, "% .\n Soit",EffectifsCC[4],"ménages sur les", Population, "ménages étudiés." )

#-------------------------------------
# 2- Déterminer le mode et l'étendue.
#-------------------------------------

#----------
# 2.1-Mode
#----------

Mode <- Effectifs[1]
Minimum <- Effectifs[1]
i <- 2
for(i in 2:9){if (Effectifs[i]> Mode){Mode <- Effectifs[i]}else if (Effectifs[i]<Minimum){Minimum <- Effectifs[i]}}
cat("Le Mode correspond à la valeur ou éffectif (Xi)la plus importante.\nLa majoritée des ménages interroger ont : " , Mode, "enfants.")

#--------------
# 2.2- Etendue
#--------------

Maximum<-Mode
Etendue <- Maximum - Minimum
cat("L'étendue de la série est de :" ,Etendue, ".")

#------------------------------------------------
#3-Calculer le nombre moyen d'enfants par ménage.
#------------------------------------------------

i <- 1
Produits_Xi_Ni <- 0
for (i in 1:9){
	Produits_Xi_Ni <- Produits_Xi_Ni+ Nombre_Enfants[i]*Effectifs[i]
}
x <- round(Produits_Xi_Ni/Population, 0)
cat("En moyenne, chaque ménage à ", x, "enfants.")
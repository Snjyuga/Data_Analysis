#========================================================================================
# Analyse de données avec R
# Population: Salariés à temps complet des secteurs privé et semi-public, à l'exception
#		  de l'agriculture et des personnels domestiques, 1979 et 1983.
# Caractère étudié: Salaires
# Nature: quantitatif discret
# Objectifs:
#		1-Déterminer les salaires médians pour 1979 et 1983.
#		2-Calculer l'écart interdécile (EID) pour 1979 et 1983.
#		3-Calculer pour 1979 et 1983, le rapport EID/médiane. Qu'en déduit-
#		on sur la disparité des salaires durant ces deux années?
#========================================================================================


#---------------------------------
# Redaction du tableau statistique
#---------------------------------

Deciles <- c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9")

Salaires1979 <- c(24920 , 29290 , 32620 , 36150 , 39560 ,44100 , 49770 , 58270 , 75740 )

Salaires1983 <- c(40490 , 47410 , 53220 , 58630 , 64370 , 71520 , 80660 , 94120 , 122900)

Tableau <- data.frame( Deciles , Salaires1979 , Salaires1983)

#-----------------------------------------------------
# 1- Déterminer les salaires médians pour 1979 et 1983
#------------------------------------------------------

Mediane1979 <- Salaires1979[5]

Mediane1983 <- Salaires1983[5]

#--------------------------------------------------
# 2- Calculer l'écart interdécile pour 1979 et 1983
#--------------------------------------------------

EID_1979 <- Salaires1979[9] - Salaires1979[1]

EID_1983 <- Salaires1983[9] - Salaires1983[1]

#-----------------------------------------------------------
# 3- Calculs des rapports EID / Mediane, faire une déduction
#    sur la disparité des salaires durant ces deux années
#-----------------------------------------------------------

Rapport1979 <- EID_1979 / Mediane1979
Rapport1983 <- EID_1983 / Mediane1983


#Dans ma série, l'intervalle qui concentre 80% des salaires 
#(autour de la médiane) à une largeur égale à environ 1,28 fois la médiane.
# Cela indique une dispersion relativement importante autour de la médiane.





#=========================================
# TD 1: SIMULALTION STOCHASTIQUE
#=========================================



#--------------------------------------------------------------------------
# Exercice 2: X(Omega) = {1, 34, 48}. P(X=1)=2/3, P(X=34)=1/6, P(X=48)=1/6
#-------------------------------------------------------------------------


Singletons <- c(1, 34, 48)
Probabilites <- c(2/3, 1/6, 1/6)
Simulation <- sample (Singletons, size = 100000,replace = TRUE, prob = Probabilites)
Effectif <- table(Simulation)
Frequence <- prop.table(Effectif)
Data <- data.frame(Effectif, Frequence)
View(Data)
Data$Simulation.1 <- NULL



#------------------------------------------------------------------
# Exercice3: On jette une paire de dés bien équilibrés et à chaque
# issue, on attribue la somme X des points obtenues.
	1- Donner la distribution de la variable aléatoire X.
	2- Simuler cette variable aléatoire X.
#------------------------------------------------------------------

#----------------------------------------------
# 1- Distribution de la v.a X dans un dataframe
#------------------------------------------------

Die1 <- seq(from = 1, to = 6)
Die2 <- rep(Die1, times = 1) 
Lignes <- length(Die1)
Colonnes <- length(Die1)

Table_Resultats <- get_resultats(Die1,Die2,Table_Resultats,Lignes,Colonnes)

Xi <- get_univers(Xi, Lignes, Colonnes, Table_Resultats) # Xi vecteur correspondant à X(Omega)

Pi <- get_probas(Pi, Xi, Lignes, Colonnes, Table_Resultats) # vecteur contenant les degrés de vraisssemblance

Distribution <- data.frame(Xi,Pi)

#----------------------------
# 2- Simulation de la v.a  X.
#----------------------------

Simulation <- sample(Xi, size= 100000, replace= TRUE, prob= Pi)
Simulation
Effectif <- table(Simulation)
Frequence <- prop.table(Effectif)
Data <- data.frame(Effectif, Frequence)
View(Data)
Data$Simulation.1 <- NULL


#---------------------------------------------------------------------------------
#Exercice 4:
#	 On jette trois fois de suite une pièce de monnaie bien équilibrée.
#	1. Donner la distribution de la variable aléatoire X qui donne le nombre de
#	faces obtenus.
#	2. Simuler cette variable aléatoire X.
#---------------------------------------------------------------------------------

#----------------------------------------------
# 1- Distribution de la v.a X dans un dataframe
#------------------------------------------------

Xi <- c("PPP","PPF", "PFP", "FPP", "FFF", "FFP", "FPF", "PFF")
Pi <- rep(1/8, times= length(Xi))
Distribution <- data.frame(Xi,Pi)
View(Distribution)
Simulation <- sample(Xi, size= 10000, replace= TRUE, prob= Pi)
Simulation
Effectif <- table(Simulation)
Frequence <- prop.table(Effectif)
Data <- data.frame(Effectif, Frequence)
View(Data)
Data$Simulation.1 <- NULL


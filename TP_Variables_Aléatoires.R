#---------------------------------------------------
#Travaux Pratiques sur R
# Probabilités et Simulation
# Applications aux Lois de Variables Aléatires
#--------------------------------------------------

#---------------------------------------------------------------------------------
#	Exercice 1: Loi Uniforme Continue
# -Contexte:
#La durée (en heures) d'attente d'un client dans un service administratif
#suit une loi uniforme sur l'intervalle [0,2]. On note X cette variable aléatoire.
#---------------------------------------------------------------------------------

#1.1 Rappels théoriques
# a) Donner la densité de probabilité de X
a <- 0
b <- 2
Densite <- 1/b-a

# b) Calculer théoriquement E(X) et Var(X)

Esperance_Theorique <- (a+b)/2
Variance_Theorique <- (b-a)/12   
# c) Donner l'expréssion de la fonction de répartition F(x)= P(X<=x)
# Si x < 0 F(x)=0; Si x £ [0,2] F(x) = P(X <= x) = 0.5x; Si x > 2 F(x)= 1


# 1.2) Simulation et vérification
# a) Simuler un échantillon de n = 1000 durées d'attente.

Attentes <- round(runif(1000,min = 0, max = 2),1)
View(Attentes)
# b) Calculer la moyenne et la variance empiriques

Moyenne_Empirique <- mean(Attentes)
Variance_Empirique <- var(Attentes)

#1.3) Visualisation
# a) Créer un histogramme de vos données avec l'option probability = TRUE

histogram <- hist(
	Attentes,
	prob = TRUE,
	main = "Histogramme avec densité théorique",
	xlab = "Modalites sous forme de classes",
	ylab = "Densité",
	col = "lightblue",
	border = "white",
	breaks = 30,
	ylim = c(0, 1))
abline(h = 0.5, # La densité horizontale à superposer
       col = "red", # Couleur de la ligne
       lwd = 2, # Épaisseur de la ligne
       lty = 2) # Type de ligne (ici, en pointillés)



classes <- histogram$breaks
effectifs <- histogram$counts
frequences <- effectifs/ sum(effectifs)
densites <- frequences/0.05

classes
effectifs
frequences
mean(densites)


# 1.4) Calculs de probabilités: théoriquement et empiriquement

	#Calcul de l'ECDF pour les calculs de probabilités empiriques
ECDF <- ecdf(Attentes)

# a)P(X <= 1) = 0.5*1 = 0.5 probabilité théorique
Proba_empirique_a = ECDF(1) 

# b) P(X <= 1.5) = 0.5*1.5 = 0.75 probabilité théorique
Proba_empirique_b = ECDF(1.5)

# c) P(0.5 <= X <= 1.5) = (0.5*1.5) - (0.5*0.5) = 0.75 - 0.25 = 0.5 probabilité théorique
    Proba_empirique_c =  ECDF(1.5) - ECDF(1) 

# 1.5) Loi des Grands Nombres
# a) 

Attentes_10 <- round(runif(10,min = 0, max = 2),1)
Attentes_50 <- round(runif(50,min = 0, max = 2),1) 
Attentes_100 <- round(runif(100,min = 0, max = 2),1)
Attentes 
Attentes_5000 <- round(runif(5000,min = 0, max = 2),1)
Echantillons <- c(10,50,100,1000,5000)

Moyenne_10 <- mean(Attentes_10)
Moyenne_50 <- mean(Attentes_50)
Moyenne_100 <- mean(Attentes_100)
Moyenne_Empirique
Moyenne_5000 <- mean(Attentes_5000)
Moyenne <- c(Moyenne_10,Moyenne_50,Moyenne_100,Moyenne_Empirique,Moyenne_5000)

# b) Graphique montrant l'évolution de la moyenne empirique en fonction de n

plot(Echantillons, Moyenne,
     type = "b",
     main = "Évolution de la moyenne empirique en fonction de n",
     xlab = "Taille des échantillons",
     ylab = "Moyenne des échantillons",
     col = "darkblue",
     pch = 19,
     lwd = 2)


# Ajout d'une grille pour une meilleure lisibilité.
grid(col = "lightblue", lty = "dotted")


# c) Que constatez-vous? Quel théorème est illustré?








#---------------------------------------------------------------------------------
#	Exercice 2: Loi Uniforme Exponentielle
# -Contexte:
#Le temps (en minutes) entre deux arrivées de clients dans une banque suit une loi
#exponentielle de paramètre lambda = 0.5 (soit en moyenne une arrivée toute les 2 minutes)
#---------------------------------------------------------------------------------
2.1) Rappels théoriques
a) Donner la densité de probabilité f(x) de X

lambda <- 0.5

f(x) = lambda*e^(-lambda*x) si x >= 0 ; f(x) = 0 si x < 0

Densite_theorique <- lambda*exp(-lambda*2) 

b) calculer théoriquement E(X) et Var(X)

Esperance_theorique <- 1 / lambda
Variance_theorique <- 1 / (lambda)^2

c) Donner l'expréssion de la fonction de répartition F(x)

F(x) = P(X <= x) = 1 - e^(-lambda*x)
Fonction_theorique <- 1 - exp(-lambda*2)

2.2) Simulation et visualisation
a) Simuler n = 5000 temps d'attente

n_5000 <- round(rexp(5000, rate = lambda),1)

classes <- histogram$breaks
effectifs <- histogram$counts

b) Vérifier empiriquement l'éspérance et la variance

Esperance_empirique <- mean(n_5000)
Variance_empirique <- var(n_5000)

c) Creer un histogramme avec la densité théorique superposée

histogram <- hist(   
	n_5000,
	prob = TRUE,
	main = "Histogramme avec densité théorique",
	xlab = "Modalites sous forme de classes",
	ylab = "Densité",
	col = "lightblue",
	border = "white",
	breaks = 50,
	ylim = c(0, 0.8))
abline(h = Densite_theorique, # La densité horizontale à superposer
       col = "red", # Couleur de la ligne
       lwd = 2, # Épaisseur de la ligne
       lty = 2) # Type de ligne (ici, en pointillés)

d) Tracer la fonction de répartition empirique et la comparer avec la CDF théorique
View(table(n_5000))
Fonction_empirique <- ecdf(n_5000)

plot(Fonction_empirique,
     main = "Fonction de répartition empirique",
     xlab = "Temps d'attentes",
     ylab = "Probabilité cumulée",
     col = "darkblue",
     lwd = 2)
curve(pexp(x, rate = lambda),
      add = TRUE,
      col = "red",
      lty = 2)

legend("bottomright",
       legend = c("Fonction empirique", "Fonction théorique"),
       col = c("darkblue", "red"),
       lty = c(1, 2),
       lwd = c(2, 1.5))

2.3) Probabilités pratiques
a) Quelle est la probabilité d'attendre plus de 3 minutes entre deux clients?

Proba_theorique_a <- 1 - (1 - exp(-lambda*3))           

Proba_empirique_a <- 1 - Fonction_empirique(3)

b) Quelle est la probabilité d'attente entre 1 et 4 minutes?

Proba_theorique_b <- (1 - exp(-lambda*4)) - (1 - exp(-lambda*1))
Proba_empirique_b  <-  Fonction_empirique(4) - Fonction_empirique(1)

c) Si on observe déjà 2 minutes d'attente, quelle est la probabilité d'attendre
   encore au moins 3 minutes? (Propriété sans mémoire)

Proba_theorique_c <- exp(-lambda*3)
Proba_empirique_c <- 1 - Fonction_empirique(3)

2.4) Propriété sans mémoire
a)
# Je n'ai pas capté!!!

b)Calculer empiriquement P(X>5|X>2) en utilisant votre échantillon

Proba_2_4b <- 1 - Fonction_empirique(3)

c) Calculer empiriquement P(X > 3)

Proba_2_4c <-  1 - Fonction_empirique(3)

# Démonstration du résultat de la probabilité de propriété sans mémoire


2.5) Somme de variables exxponentielles

a) simuler 1000 fois la somme S = X1+X2+X3+X4+X5

echantillon_S <- round(rexp(1000, rate = 2.5),1)

b) calculer E(S) et Var(S) empiriquement

Esperance_empirique_S <- mean(echantillon_S)
Var_empirique_S <- var(echantillon_S)

c) comparer avec les valeurs théoriques: E(S) = 5E(X) et Var(S)= 5Var(X)


d) Créer un histogramme de S. Quelle loi semble suivre S?

hist(
	echantillon_S,
	prob = TRUE,
	main = "Histogramme avec densité théorique",
	xlab = "Modalites sous forme de classes",
	ylab = "Densité",
	col = "lightblue",
	border = "white",
	breaks = 50,
	ylim = c(0, 0.8),
	xlim = c(0, 3.5))


#---------------------------------------------------------------------------------
#	Exercice 3: Loi Normale et Standardisation
# -Contexte:
#Les notes d'un examen suivent une loi normale de moyenne 12 et d'écart-type 3.
#On note X la note d'un étudiant choisi au hasard.
#-----------------------------------------------------------------------------

#3.1) Loi normale standard

# a) Simuler n 10000 valeurs de Z~N(0,1)

echantillon_10000 <- rnorm(10000, mean = 0, sd = 1)

#b) Vérifier empiriquement que E(Z) = 0 et Var(Z) = 1

Esperance_empirique <- mean(echantillon_10000 )
Variance_empirique <- var(echantillon_10000 )

#c) Créer un histogramme avec la courbe 

hist(
	echantillon_10000,
	prob = TRUE,
	main = "Histogramme avec la courbe de la densité théorique",
	xlab = "Modalites sous forme de classes",
	ylab = "Densité",
	col = "lightblue",
	border = "white",
	breaks = 50,
	ylim = c(0, 0.4),
	xlim = c(-3.5, 3.5))

abcisses <- seq(min(echantillon_10000), max(echantillon_10000), length.out = 100)
densites <- dnorm(abcisses, mean = 0, sd = 1)

lines(abcisses, densites,
      col = "red",
      lwd = 2)

abline(v = 0, col = "darkblue", lty = 2, lwd = 1.5)

abline(v = c(0 - 1, 0 + 1),
       col = "darkorange", lty = 3, lwd = 1.5)

abline(v = c(0 - 2*1, 0 + 2*1),
       col = "grey", lty = 3, lwd = 1.5)

abline(v = c(0 - 3*1, 0 + 3*1),
       col = "pink", lty = 3, lwd = 1.5)

#d) Calculer empiriquement P(-1 < Z < 1), P(-2 < Z < 2), P(-3 < Z < 3)

fonction_empirique <- ecdf(echantillon_10000)
proba1 <- fonction_empirique(1) - fonction_empirique(-1)
proba2 <- fonction_empirique(2) - fonction_empirique(-2)
proba3 <- fonction_empirique(3) - fonction_empirique(-3)

# 3.2) Loi normale des notes

#a) Simuler 10000 notes d'examen suivant N(12,9)

echantillon3.2 <- round(rnorm(10000,mean = 12, sd = 3),1)
echantillon3.2
# b) Vérifier empiriquement les paramètres

Esperance_empirique3.2 <- mean(echantillon3.2)
Variance_empirique3.2 <- var(echantillon3.2)
Esperance_empirique3.2
Variance_empirique3.2

# c) Créer un histogramme avec la densité théorique

hist(
	echantillon3.2,
	prob = TRUE,
	main = "Histogramme avec la courbe de la densité théorique",
	xlab = "Modalites sous forme de classes",
	ylab = "Densité",
	col = "red",
	border = "white",
	breaks = 50,
	ylim = c(0, 0.4),
	xlim = c(0, 3.5))

abcisses <- seq(min(echantillon3.2), max(echantillon3.2), length.out = 100)
densites <- dnorm(abcisses, mean = 12, sd = 3)

lines(abcisses, densites,
      col = "lightblue",
      lwd = 2)

# Calculer les probabilités suivantes (théoriquement et empiriquement):

# d) P(X >= 10): probabilité d'avoir la moyenne
Fonction_empirique <- ecdf(echantillon3.2)

proba_theorique_d <- 1 - pnorm(10,mean = 12, sd = 3)
proba_theorique_d

proba_empirique_d <- 1 - Fonction_empirique(10)
proba_empirique_d

# e) P (X >= 16): probabilité d'avoir une très bonne note

proba_theorique_e <- 1 - pnorm(16, mean = 12, sd = 3)
proba_theorique_e

proba_empirique_e <- 1 - Fonction_empirique(16)
proba_empirique_e

# f) P(X >= 6): probabilité d'échouer gravement

proba_theorique_f <- pnorm(6,mean = 12, sd = 3)
proba_theorique_f

proba_empirique_f <- Fonction_empirique(6)
proba_empirique_f 


# 3.4) Quantiles et seuils
#a) Déterminer la note minimale a pour faire partie des 10% meilleurs étudiants
#   c'est-à dire P(X > a) = 0.1
P(X > a) = 1 - phi(a) = 0.1 => phi(a) = 0.9
a - 12 / 3 = 1.28 => a = 15.84

#b) Déterminer l'intervalle [b,c] contenant 95% des notes (centré sur la moyenne)
test = c(12 - 2*3, 12 + 2*3)

X <- hist(
	echantillon3.2,
	main = "Histogramme des notes en fonction des éffectifs",
	xlab = "Classes",
	ylab = "Effectifs",
	col = "lightblue",
	border = "white",
	breaks = 50,
	ylim = c(0, 683),
	xlim = c(2.5, 23.5))

abline(v = 12, col = "darkblue", lty = 2, lwd = 1.5)

abline(v = c(12 - 3, 12 + 3),
       col = "darkorange", lty = 3, lwd = 1.5)

abline(v = c(12 - 2*3, 12 + 2*3),
       col = "grey", lty = 3, lwd = 1.5)

abline(v = c(12 - 3*3, 12 + 3*3),
       col = "pink", lty = 3, lwd = 1.5)

classes <- X$breaks
effectifs <- X$counts

# c) Vérifier empiriquement ces valeurs avec votre échantillon
proba_empirique_0_1 <- Fonction_empirique(15.84)
proba_empirique_0_95 <- Fonction_empirique(18)-Fonction_empirique(6)

#3.5) Théorème Central limite

# a) Simuler 2000 échantillons de tailles n = 25 de notes


Echantillons <- replicate(2000, expr = sample(rnorm(25,mean = 12, sd = 3)))

# b) Pour chaque échantillon, calculer la moyenne X(25)

Moyennes <- numeric(2000)
for(i in 1:2000){
	Moyennes[i] <- sum(Echantillons[i]) / length(Echantillons[i])
}
View(Moyennes)

# c) Créer un histogramme de ces 2000 moyennes

Y <- hist(
	Moyennes,
	prob = TRUE,
	main = "Histogramme des moyennes",
	xlab = "Classes",
	ylab = "Effectifs",
	col = "yellow",
	border = "white",
	breaks = 50,
	ylim = c(0, 0.15),
	xlim = c(1, 24))

abcisses <- seq(min(Moyennes), max(Moyennes), length.out = 100)
densites <- dnorm(abcisses, mean = 12, sd = 3)

lines(abcisses, densites,
      col = "green",
      lwd = 2)
classes <- Y$breaks
effectifs <- Y$counts


# d)Quelle loi devrait suivre X(25)? Vérifier graphiquement.
Pour vérifier graphiquement que X25 suit une loi normal il faut tracer l'histogramme
des densités avec la courbe en cloche


# e) Calculer P(X25 > 13) théoriquement et empiriquement

proba_theorique_3_5_e <- 1 - pnorm(13,mean = 12, sd = 3)
proba_theorique_3_5_e

Fonction_empirique <- nu(2000)
for(i in 1:2000){
	Fonction_empirique[i] <- ecdf(Echantillons[i])
}

Fonction_empirique_test <- ecdf(Echantillons[1])

Fonction_empirique <- ecdf( Echantillons[i] )




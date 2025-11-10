#===================================
# Fontion utile au script principal
#===================================

#----------------------------------------------------------------------------------
# fonction qui me retourne la proportion,d'employés qui perçoivent au moins 400.000 F CFA
#------------------------------------------------------------------------------------------

get_proportion <-  function(Bornes_inf,Frequences){
	n <- length(Frequences)
	i <- 1
	proportion <- 0
	for (i in 1:n){
		if (Bornes_inf[i] >= 40){
			 proportion <- proportion + Frequences[i]

		}
	}
	proportion <- proportion * 100
	return (proportion)
}

#----------------------------------------
# fonctions calcul des proportions d'avis
#-----------------------------------------

get_proportion_a <- function(NombreD_etoiles,FrequencesCC){
	i <- 1
	for(i in 1:length(NombreD_etoiles)){
		if(NombreD_etoiles[i] == 2){
			a <- FrequencesCC[i-1]*100
		}
	}
	proportion_a <- paste("La proportion d'avis notés d'au plus 2 étoiles est de : ", a,"%.")
	return(proportion_a)
}

get_proportion_b <- function(NombreD_etoiles,Frequences){
	i <- 1
	n <- length(NombreD_etoiles)
	for(i in 1:n){
		if(NombreD_etoiles[i] == 3){
			b <- (1 - FrequencesCC[i])*100
		}
	}
	proportion_b <- paste("La proportion d'avis notés d'au moins 3 étoiles est de : " , b, " % . " )
	return(proportion_b)
}

get_proportion_c <- function(NombreD_etoiles,Frequences){
	i <- 1
	n <- length(NombreD_etoiles)
	c <- 0
	for(i in 1:n){
		if(NombreD_etoiles[i] > 1 && NombreD_etoiles[i] < 4){
		c <- c + Frequences[i]
		}
	}
	c <- c *100
	proportion_c <- paste("La proportion d'avis notés entre 1 et 4 étoiles est de : " , c, " % . " )
	return(proportion_c)
} 

#------------------------------
#fonction calcul des quartiles 		(BUG A GERER!!!!!)
#------------------------------
#'
#' Cette fonction prend trois vecteurs en entrée et renvoie les quartiles de la série étudiée.
#'
#' @param vecteur Un vecteur numérique (Bornes inférieures des classes)
#' @param vecteur Un vecteur numérique (Bornes supérieures des classes)
#' @param vecteur Un vecteur numérique (Fréquences cumulées croissantes)
#'
#' @return un vecteur numérique (reel) Un vecteur contenant les quartiles 
#'
#' @examples
#' x <- c(2,6,10); y <- c(4,8,12); z <- c(0.2,0.37,1)
#' get_quartile <- function(x,y,z)
#' # [1] 3.3
#'
get_quartiles <- function(vecteur1,vecteur2,vecteur3){
	alpha <- c(0.25,0.5,0.75)
	quartile <- seq(from = 0, to =0, length.out = 3)
	j <- 1
	for(j in 1:length(alpha)){
		i <- 1
		while( i <= length(vecteur2)){
			if ((vecteur3[i] >= alpha[j]) & (vecteur3[i]== vecteur3[1])){
				Fi <- vecteur3[i]
				Fi_1 <- 0
				Fii <- vecteur3[i +1]
				xi <- vecteur1[i]
				xii <- vecteur2[i]
				break
			} else if((vecteur3[i] >= alpha[j]) & (vecteur3[i] == vecteur3[length(vecteur3)])){
				Fi <- vecteur3[i]
				Fi_1 <- vecteur3[i - 1]
				Fii <- 1
				xi <- vecteur1[i]
				xii <- vecteur2[i]
				break
			} else {
				Fi <- vecteur3[i]
				Fi_1 <- vecteur3[i - 1]
				Fii <- vecteur3[i +1]
				xi <- vecteur1[i]
				xii <- vecteur2[i]
				break
			}
			i <- i + 1
		} 
		quartile[j] <- xi + (xii - xi)*(Fi - Fi_1) / (Fii - Fi_1)
	}
	return(quartile)
}

#---------------------------------
#fonction qui calcule les déciles     (BUG A GERER!!!!!)
#---------------------------------
#'
#' Cette fonction prend trois vecteurs en entrée et renvoie les déciles de la série étudiée.
#'
#' @param vecteur Un vecteur numérique (Bornes inférieures des classes)
#' @param vecteur Un vecteur numérique (Bornes supérieures des classes)
#' @param vecteur Un vecteur numérique (Fréquences cumulées croissantes)
#'
#' @return un vecteur numérique (reel) Un vecteur contenant les déciles de la série
#'
#' @examples
#' x <- c(2,6,10); y <- c(4,8,12); z <- c(0.2,0.37,1)
#' get_quartile <- function(x,y,z)
#' # [1] 3.3
#'
get_deciles <- function(vecteur1,vecteur2,vecteur3){
	alpha <- seq(from = 0.10,to = 0.90, by = 0.1)
	deciles <- seq(from = 0, to =0, length.out = 9)
	j <- 1
	for(j in 1:length(alpha)){
		i <- 1
		while( i <= length(vecteur2)){
			if(vecteur3[i] >= alpha[j]){
				Fi <- vecteur3[i]
				Fi_1 <- vecteur3[i - 1]
				Fii <- vecteur3[i +1]
				xi <- vecteur1[i]
				xii <- vecteur2[i]
				break
			}
			i <- i + 1
		} 
		deciles[j] <- xi + (xii - xi)*(Fi - Fi_1) / (Fii - Fi_1)
	}
	return(deciles)
}

#-------------------------------------------
# fonction qui calcul le coefficient de yule 
#-------------------------------------------

get_coef_yule <- function (Quartiles){
	Q1 <- Quartiles[1]
	Q2 <- Quartiles[2]
	Q3 <- Quartiles[3]
	C <- ((Q3 - Q2)-(Q2 - Q1))/(Q3 - Q1)
	if(C < 0){
		comment <- paste("Il y'a étalement à gauche et assymétrie à droite de la série, Coef Yule:", C)
	}
	else if (C == 0){
		comment <- paste("La distribution est parfaitement symétrique car Coef Yule =" , C)
	}
	else {
		comment <- paste("Il y'a étalement à droite et assymétrie à gauche de la série, Coef Yule:", C)
	}
	return(comment)
}

#------------------------------------------
# fonction calcul et commente le dispersion
#------------------------------------------

get_variation <- function(Variance,Moyenne){
	cov_var <- (sqrt(Variance) / Moyenne)*100
	if (cov_var <15){
		message <- paste("La série est homogène avec une faible disparité des valeurs de la série autour de la moyenne.")
	}
	else if (cov_var >= 15 && cov_var <= 30){
		message <- paste("La série est relativement homogène(dispersion modérée autour de la moyenne).")
	}
	else {
		message <- paste ("La série est extremement dispersée et hétérogène. La moyenne n'est pas représentative de l'ensemble des avis(les notes sont trop dispersées autour d'elle).")
	}
	return(message)
}

#-------------------------------------
# fonction pour calculer la population
#-------------------------------------
#'
#' Cette fonction prend un vecteur en entrée et renvoie la somme totale.
#'
#' @param vecteur Un vecteur numérique
#'
#' @return entier Un entier représentant la somme des valeurs du vecteur
#'
#' @examples
#'get_population(c(4,9,5))
#' # [1] 18
#'
get_population <- function(vecteur){
	# Vérification des entrées
	if (length(vecteur) == 0) {
		stop("Le vecteur d'entrée est vide. Impossible de calculer les fréquences.")
  	}
  
  	if (!is.numeric(vecteur)) {
    		stop("Le vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur < 0)) {
    		warning("Certaines valeurs du vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	total <- 0 
	n <- length(vecteur)
	i <- 1
	for (i in 1:n){
		total <- total + vecteur[i]
	}
	return(total)
}

#------------------------------------------------
# fonction pour calculer les frequences relatives
#-------------------------------------------------

#'
#' Cette fonction prend un vecteur en entrée et renvoie les fréquences relatives.
#' Si l'argument 'Population' n'est pas fourni, la fonction la calcule automatiquement.
#'
#' @param vecteur Un vecteur numérique 
#' @param entier Un entier (somme des valeurs du vecteur d'entrée). Si NULL, la fonction le calcule automatiquement.
#'
#' @return vecteur Un vecteur numérique des fréquences relatives
#'
#' @examples
#' get_frequences_rel(c(10,27,98))
#' # [1] 0.07 0.2 0.73 
#'
get_frequences_rel <- function(vecteur,Population = NULL){
	# Vérification des entrées
	if (length(vecteur) == 0) {
		stop("Le vecteur d'entrée est vide. Impossible de calculer les fréquences.")
  	}
  
  	if (!is.numeric(vecteur)) {
    		stop("Le vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur < 0)) {
    		warning("Certaines valeurs du vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	# Population : valeur par défaut si NULL
	if (is.null(Population)) {
		Population <- sum(vecteur)
    		message("Population non fournie → calcul automatique : ", Population)
  	}
  
  	if (Population <= 0) {
    		stop("La population doit être strictement positive.")
  	}

	n <- length(vecteur)
	frequences <- numeric(n)
	for(i in 1:n){
	frequences[i] <- vecteur[i] / Population
	}
	return(frequences)
}

#-------------------------------------------------------------------------------
# fonction pour calculer les valeurs cumulées croissantes d'un vecteur numérique
#-------------------------------------------------------------------------------

#' Cette fonction prend un vecteur numérique et renvoie les valeurs cumulées croissantes.
#'
#' @param vecteur Un vecteur numérique
#'
#' @return vecteur Un vecteur des sommes cumulées
#'
#' @examples
#'get_cumul_c <- (c(1,2,3))
#' # [1] 1 3 6
#'
get_cumul_c <- function(vecteur){
		# Vérification des entrées
	if (length(vecteur) == 0) {
		stop("Le vecteur d'entrée est vide. Impossible de calculer les cumuls croissants.")
  	}
  
  	if (!is.numeric(vecteur)) {
    		stop("Le vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur < 0)) {
    		warning("Certaines valeurs du vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	n <- length(vecteur)
	cumul <- numeric(n)
	cumul[1] <- vecteur[1]
	for(i in 2:n){
		cumul[i] <- cumul[i-1] + vecteur[i]
	}
	return(cumul)
}

#----------------------------------
# fonction pour calculer la moyenne
#----------------------------------
#'
#' Cette fonction prend deux vecteurs numériques et un entier en entrée et renvoie la moyenne.
#' Si l'argument 'Population' n'est pas fourni, la fonction la calcule automatiquement.
#'
#' @param vecteur Un vecteur numérique (centres des classes ou les modalités pour une variable quantitative)
#' @param vecteur Un vecteur numérique (vecteur :Effectifs)
#' @param entier Un entier correspondant à la population
#'
#' @return réel Un réel correspondant à la moyenne arithmétique
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5)
#' get_moyenne <- (x,y)
#' # [1] 11.25
#'
get_moyenne <- function(vecteur1,vecteur2,Population = NULL){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)){
		stop("Les deux vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la moyenne.")
  	}
  
  	if (!is.numeric(vecteur1)) {
    		stop("Le premier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur1 < 0)) {
   		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la moyenne.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	# Population : valeur par défaut si NULL
	if (is.null(Population)) {
		Population <- sum(vecteur2)
    		message("Population non fournie → calcul automatique : ", Population)
  	}
  
  	if (Population <= 0) {
    		stop("La population doit être strictement positive.")
  	}

	i <- 1
	vec1_vec2 <- 0
	 n <- length(vecteur1)
	for(i in 1:n){
		vec1_vec2 <- vec1_vec2 + (vecteur1[i] * vecteur2[i])
	}
	moyenne <- vec1_vec2 / Population
	return(moyenne)
}

#------------------------------
#fonction calcul de la variance
#------------------------------
#'
#' Cette fonction prend deux vecteurs numériques(le premier correspond aux centres de classes et le second aux effectifs) 
#' et deux entier (le premier représente la moyenne et le second à la population )en entrée et renvoie la variance de la série.
#' Si l'argument 'Population' n'est pas fourni, la fonction la calcule automatiquement.
#'
#' @param vecteur Un vecteur numérique (Centre de classes ou modalites Xi d'une Variable discrète)
#' @param vecteur Un vecteur numérique (Effectifs correspondants)
#' @param reel Un entier correspondant à la moyenne arithmétique de la série
#' @param entier Un entier correspondant à la population
#'
#' @return réel Un réel correspondant à la variance
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5); moyenne <- 3; population <- 30
#' get_var <- (x,y,moyenne,population) 
#' # [1] 5.36
#'
get_var <- function(vecteur1,vecteur2,reel = NULL,Population = NULL){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)){
		stop("Les deux vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la variance.")
  	}
  
  	if (!is.numeric(vecteur1)) {
    		stop("Le premier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur1 < 0)) {
    		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la variance.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	# Moyenne : valeur par défaut si NULL
	if (is.null(reel)){
	reel <- get_moyenne(vecteur1,vecteur2,Population)
	}
	if (reel <= 0) {
    		stop("La moyenne arithmétique doit être strictement positive.")
  	}
	# Population : valeur par défaut si NULL
	if (is.null(Population)) {
		Population <- sum(vecteur2)
    		message("Population non fournie → calcul automatique : ", Population)
  	}
  
  	if (Population <= 0) {
    		stop("La population doit être strictement positive.")
  	}
	i <- 1
	produit <- 0
	n <- length(vecteur1)
	for(i in 1:n){
	produit <- produit + vecteur2[i] *(vecteur1[i]  - reel)^2
	}
	Koening <- produit / Population
	return(Koening)
}

#---------------------------------------------------------
#Calcul de la médiane à partir des classes et FréquencesCC
#----------------------------------------------------------
#'
#' Cette fonction prend trois vecteurs numériques(le premier correspond aux bornes inférieures des classes,  
#' le second aux bornes supérieures et le dernier aux fréquences relatives)en entrée et renvoie la médiane de la série.
#'
#' @param vecteur Un vecteur numérique (Bornes inférieures des classes)
#' @param vecteur Un vecteur numérique (Bornes supérieures des classes)
#' @param vecteur Un vecteur numérique (Fréquences cumulées croissante de la série)
#'
#' @return reel Un réel correspondant à la médiane
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5); z <- c((5,4,3)
#' get_mediane <- (x,y,z) 
#' # [1] 5.36
#'
get_mediane <- function(vecteur1,vecteur2,vecteur3){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)||length(vecteur1)!=length(vecteur3)||length(vecteur2)!=length(vecteur3)){
		stop("Les trois vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur1)) {
    		stop("Le premier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur1 < 0)) {
    		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	if (length(vecteur3) == 0) {
		stop("Le dernier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur3)) {
    		stop("Le dernier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur3 < 0)) {
    		warning("Certaines valeurs du dernier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	y <- 0.5
	n <- length(FrequencesCC)
	i <- 1
	while(i <= n){
		if(FrequencesCC[i] >= y){
			y2 <- FrequencesCC[i]
			x2 <- Bornes_sup[i]
			x1 <- Bornes_inf[i]
			y1 <- FrequencesCC[i - 1]
			break
		}
		i <- i + 1
	}
	x <- x1 + (x2 - x1)*(y - y1)/(y2 - y1)
	return(x)
}

#------------------------------------------------------------------------------------------------------------------------
# fonction qui calcul le mode variable continue (classes d'amplitudes différentes)par la méthode d'interpolation linéaire
#------------------------------------------------------------------------------------------------------------------------
#'
#' Cette fonction prend trois vecteurs numériques(le premier correspond aux bornes inférieures des classes,  
#' le second aux bornes supérieures et le dernier aux densités)en entrée et renvoie le mode de la série.
#'
#' @param vecteur Un vecteur numérique (Bornes inférieures des classes)
#' @param vecteur Un vecteur numérique (Amplitudes)
#' @param vecteur Un vecteur numérique (Densités)
#'
#' @return reel Un réel correspondant au mode
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5); z <- c((5,4,3)
#' get_mode_continue <- (x,y,z) 
#' # [1] 5.36
#'
get_mode_continue <- function(vecteur1,vecteur2,vecteur3){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)||length(vecteur1)!=length(vecteur3)||length(vecteur2)!=length(vecteur3)){
		stop("Les trois vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur1)) {
    		stop("Le premier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur1 < 0)) {
    		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	if (length(vecteur3) == 0) {
		stop("Le dernier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur3)) {
    		stop("Le dernier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur3 < 0)) {
    		warning("Certaines valeurs du dernier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	i <- 1
	d <- vecteur3[1]
	for (i in 2:length(vecteur1)){
		if (vecteur3[i] > d){
			d <- vecteur3[i]
			d1 <-	d - vecteur3[i - 1]
			d2 <- d - vecteur3[i + 1] 
			xi <- vecteur1[i]
			a <- vecteur2[i]
		}
	}
	Mo <- xi + ( a)*(d1)/(d1 + d2)
	return(Mo)
}

#----------------------------------------------------------------------------------------------------------------------------------------------
fonction qui parcours le tableau statistique et renvoie le mode si on a une variable discrète ou la classe modale si on a une variable continue
#----------------------------------------------------------------------------------------------------------------------------------------------
#' 
#' Cette fonction prends en entrée deux vecteurs numériques (les modalités et les éffectifs ou les fréquences(pour les variables discrets) ou les densités(pour les variables continues))
#'
#' @param vecteur Un vecteur numérique (Modalites)
#' @param vecteur Un vecteur numérique (éffecctifs, fréquences, densités)
#' 
#' @return character Un character correspondant au mode qu'il faudra convertir si besoin
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5)
#' get_classe_modale <- (x,y,) 
#' # [1] "10"
#'
get_classe_modale <- function(vecteur1,vecteur2){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)){
		stop("Les deux vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.character(vecteur1)) {
    		warning("Le premier vecteur d'entrée est composé de caractère.")
  	}
  
  	if (any(vecteur1 < 0)) {
    		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	i <- 2
	n <- length(vecteur1)
	maximum <- vecteur2[1]
	for (i in 2:n){
		if (vecteur2[i] > maximum){
			maximum <- vecteur2[i]
			x <- vecteur1[i]
		}
	}
	return(x)
}

#---------------------------------------------------
# fonction qui calcule et stock les issues de la v.a
#----------------------------------------------------

get_resultats <- function(Die1,Die2,Table_Resultats,Lignes,Colonnes){
	Table_Resultats <- matrix(ncol= Colonnes , nrow= Lignes, byrow= TRUE)
	i <- 1
	j <- 1
	for (i in 1:Lignes){
		for (j in 1:Colonnes){
		Table_Resultats[i,j] <- Die1[i]+Die2[j]
		}
	}
	return(Table_Resultats)
}

#--------------------------------------------------------------------------------
# fonction qui parcours la matrice des issues de la v.a et stock les singletons
#-------------------------------------------------------------------------------

get_univers <- function(Xi, Lignes, Colonnes, Table_Resultats){
	Xi <- c()   
	i <- 1
	while(i <= Lignes){
		j <- 1
		while(j<= Colonnes){
			if (!(Table_Resultats[i,j]) %in% Xi){
				Xi <- c(Xi, Table_Resultats[i,j])
			}
			j <- j+1
		}
		i <- i+1
	}
	return(Xi)
}

#-------------------------------------
#fonction pour calculer les Pi
#--------------------------------------


get_probas <- function(Pi, Xi, Lignes, Colonnes, Table_Resultats){
	Pi <- rep(0, times= (length(Xi)))   
	k <- 1   
	i <- 1
	while(i <= Lignes){
		j <- 1
		while(j<= Colonnes){
			for (k in 1:(length(Xi))){
				if (Table_Resultats[i,j] == Xi[k]){
					Pi[k] <- Pi[k] +1
				}
			}
			j <- j+1
		}
		i <- i+1
	}
	Pi <- Pi / length(Table_Resultats)
	Pi <- round(Pi,2)
	return(Pi)
}






get_bernoulli <- function(){
	p <- runif(1)
	tirage <- runif(1)
	X <- 0
	if (tirage < p){
		X <- 1
	}
	else {
		X <- 0
	}
	return(X)
}


get_binomiale <- function(){
	proba_succes <- runif(1)
	tirage <- runif(3)
	n <- length(tirage)
	X <- 0
	i <- 1
	for(i in 1:n){
		if(tirage[i] < proba_succes){
			X <- X + 1		
		}
	}
	return(cat("Le nombre de succes après", n, "tirages est de :", X))
}


get_geometric <- function() {
	proba_succes <- runif(1)
	tirage <- runif(10)
	n <- length(tirage)
	X <- 0
	i <- 1
	(while i <= n){
		match(){
			X <- 
		}
	}



}






























